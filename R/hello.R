

#' Generate an RSA Key Pair for Kalshi API Authentication
#'
#' @description
#' This function generates a 2048-bit RSA key pair required for Kalshi's V2
#' API authentication. It saves the private key to a local file and prints
#' the public key to the console for easy copying into the Kalshi
#' Developer Dashboard.
#'
#' @param private_key_path Character. The file path where the private key should
#'   be saved. Defaults to `"kalshi-key-2.key"`.
#'
#' @details
#' Kalshi uses RSA-PSS signing for API requests. To use the API, you must:
#' 1. Generate a key pair using this function.
#' 2. Keep the `.key` file secure (add it to your `.gitignore`).
#' 3. Copy the public key text printed in the console.
#' 4. Log into the Kalshi Dashboard and upload the public key to receive
#'    your `Key ID`.
#'
#' @return Returns the public key PEM string invisibly.
#'
#' @family authentication
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate a key and save to current directory
#' generate_kalshi_keypair("my_kalshi_key.key")
#' }
generate_kalshi_keypair <- function(private_key_path = "kalshi-key-2.key") {


    # 1. Generate a 2048-bit RSA key
    # Kalshi requires RSA; 2048 is the standard secure length
    key <- openssl::rsa_keygen(bits = 2048)

    # 2. Save the Private Key to a file
    # This file is used by your sign_kalshi_system function
    openssl::write_pem(key, path = private_key_path)

    # 3. Extract the Public Key
    # You need to upload this string to the Kalshi API dashboard
    pubkey <- as.list(key)$pubkey
    pubkey_pem <- openssl::write_pem(pubkey)

    cli::cli_alert_success("Success!")
    cli::cli_alert_info("Private key saved to: {.file output_file} ")
    cli::cli_alert_info("\n--- COPY THE PUBLIC KEY BELOW TO KALSHI DASHBOARD ---")
    cli::cli_alert("{.val {pubkey_pem}}")

    # Return the public key string invisibly in case you want to capture it
    return(invisible(pubkey_pem))
}

# Usage:
# generate_kalshi_keypair("kalshi-key-2.key")





#' Internal RSA-PSS Signing via OpenSSL System Call
#'
#' @description
#' This is an internal helper that performs RSA-PSS signing using the system's
#' `openssl` binary. It is used to generate the `KALSHI-ACCESS-SIGNATURE`
#' required for authentication.
#'
#' @param private_key_path Character. Path to the user's RSA private key file.
#' @param timestamp Character. A millisecond Unix timestamp string.
#' @param method Character. The HTTP method in uppercase (e.g., "GET", "POST").
#' @param path Character. The API endpoint path. **Note:** Kalshi requires
#'   this to be the path *without* query parameters (e.g., use
#'   `/trade-api/v2/markets`, not `/trade-api/v2/markets?limit=10`).
#'
#' @details
#' Since the high-level R `openssl` package does not currently expose
#' RSA-PSS padding via its `signature_create` wrapper, this function
#' interfaces directly with the system's `openssl` installation.
#'
#' The signature is generated using:
#' 1. **Message**: `timestamp + method + path`
#' 2. **Algorithm**: SHA-256
#' 3. **Padding**: RSA-PSS
#' 4. **Salt Length**: Equal to the digest length (32 bytes for SHA-256).
#'
#' @return A base64-encoded string containing the RSA-PSS signature.
#'
#' @section System Requirements:
#' This function requires the `openssl` command-line tool to be installed
#' and available in the system's PATH.
#'
#' @keywords internal
#' @noRd
sign_kalshi_system <- function(private_key_path, timestamp, method, path) {
    # 1. Construct the exact message string
    msg <- paste0(timestamp, method, path)

    # 2. Create temporary files to handle binary data safely
    # We use temp files to avoid shell escaping issues with special characters
    msg_file <- tempfile()
    sig_file <- tempfile()

    # 3. Write the raw bytes to the file (crucial: no extra newlines!)
    writeBin(charToRaw(msg), con = msg_file)

    # 4. Construct the OpenSSL system command
    # -sha256: The hash algorithm
    # -sigopt rsa_padding_mode:pss : The required padding
    # -sigopt rsa_pss_saltlen:digest : Sets salt length = hash length (32 bytes), matching Python's 'DIGEST_LENGTH'

    # Note: Adjust 'openssl' to the full path if it's not in your system PATH
    openssl_cmd <- "openssl"

    args <- c(
        "dgst",
        "-sha256",
        "-sigopt", "rsa_padding_mode:pss",
        "-sigopt", "rsa_pss_saltlen:digest",
        "-sign", private_key_path,
        "-out", sig_file,
        msg_file
    )

    # 5. Execute the command
    # stdout=FALSE/stderr=FALSE keeps your R console clean
    result <- system2(openssl_cmd, args, stdout = FALSE, stderr = FALSE)

    if (result != 0) {
        stop("OpenSSL system call failed. Ensure 'openssl' is installed and the key path is correct.")
    }

    # 6. Read the binary signature and Base64 encode it
    sig_raw <- readBin(sig_file, what = "raw", n = 1000) # Read enough bytes (RSA 2048 sig is 256 bytes)
    sig_b64 <- base64_encode(sig_raw)

    # Cleanup
    unlink(c(msg_file, sig_file))

    return(sig_b64)
}



#' Authenticate a Kalshi API Request
#'
#' @description
#' This function implements the Kalshi V2 API authentication protocol. It
#' extracts the necessary metadata from an `httr2` request, generates a
#' SHA-256 RSA-PSS signature, and attaches the required headers.
#'
#' @param req An `httr2` request object.
#' @param kalshi_access_token Character. The Kalshi Key ID (UUID) provided in the Kalshi
#'   web dashboard.
#' @param private_key_path Character. The file path to the RSA private key
#'   (usually a .key or .pem file) used to sign the request.
#'
#' @details
#' The function automatically handles the Kalshi requirement that the signature
#' be generated using the base path *without* query parameters, even if the
#' request itself contains them.
#'
#' For example, if requesting `/trade-api/v2/markets?limit=10`, this function
#' will correctly sign only `/trade-api/v2/markets`.
#'
#' @return A modified `httr2` request object with `KALSHI-ACCESS-*` headers added.
#' @export
req_kalshi_authreq_kalshi_auth <- function(req, kalshi_access_token, private_key_path) {




    # 1. Generate Timestamp (Milliseconds as string)
    timestamp_ms <- base::as.character(base::round(base::as.numeric(base::Sys.time()) * 1000))

    # 2. Extract Method and Path
    parsed_url <- httr2::url_parse(req$url)

    # IMPORTANT: Kalshi V2 signs the PATH ONLY.
    # Even if there are query params in the URL, they are excluded from the msg string.
    path_to_sign <- parsed_url$path

    # Ensure path starts with a forward slash
    if (!base::grepl("^/", path_to_sign)) {
        path_to_sign <- base::paste0("/", path_to_sign)
    }

    method <- req$method %||% "GET"

    # 3. Generate the Signature
    # Calls the internal sign_kalshi_system helper
    signature <- sign_kalshi_system(
        private_key_path = private_key_path,
        timestamp        = timestamp_ms,
        method           = method,
        path             = path_to_sign
    )

    # 4. Attach Headers
    req %>%
        httr2::req_headers(
            `KALSHI-ACCESS-KEY`       = kalshi_access_token,
            `KALSHI-ACCESS-SIGNATURE` = base::as.character(signature),
            `KALSHI-ACCESS-TIMESTAMP` = timestamp_ms
        )
}


#' Check for OpenSSL System Dependency
#'
#' @description
#' This function verifies that the `openssl` command-line tool is installed
#' and accessible via the system's PATH. This tool is required for
#' RSA-PSS signing during Kalshi API authentication.
#'
#' @details
#' If `openssl` is not found, the function will throw an error with
#' installation suggestions based on the user's operating system.
#'
#' This check is typically called internally before any signing operation
#' or during package load/configuration.
#'
#' @return Returns `TRUE` invisibly if found; otherwise, throws an error.
#'
#' @family validation
#' @export
#'
#' @examples
#' \dontrun{
#' check_openssl_installed()
#' }
check_openssl_installed <- function() {


    if (Sys.which("openssl") == "") {

        cli::cli_abort("The 'openssl' command-line tool is required for Kalshi authentication but was not found.")

    }
}



#' Retrieve Kalshi Access Token from Environment
#'
#' @description
#' This function retrieves a stored Kalshi API token from the system environment.
#' If the token is not found, it prompts the user to edit their `.Renviron` file.
#'
#' @param kalshi_access_token Character. The name of the environment variable
#'   where your Kalshi API token is stored. Defaults to `"KALSHI_API"`.
#'
#' @details
#' Storing API keys in your `.Renviron` file is a best practice to avoid
#' hard-coding secrets into your scripts. This function simplifies the retrieval
#' and provides a helpful error message and workflow (via `usethis`) if the
#' key is missing.
#'
#' To set your key for the first time:
#' 1. Run this function.
#' 2. When the `.Renviron` file opens, add a line like: `KALSHI_API=your_token_here`
#' 3. Save the file and restart your R session.
#'
#' @return Returns the API token string.
#'
#' @family authentication
#' @export
#'
#' @examples
#' \dontrun{
#' # Standard usage
#' token <- get_kalshi_access_token()
#'
#' # Using a custom environment variable name
#' token <- get_kalshi_access_token("MY_PROD_KEY")
#' }
 get_kalshi_access_token <- function(kalshi_access_token="KALSHI_API"){



    if(!is.charater(kalshi_access_token)){

        cli::cli_abort("Kalshi access token must be a character not {.cls {base::class(kalshi_access_token)}}")
    }

    usethis::edit_r_environ()

    out <- Sys.getenv(kalshi_access_token)

    if (out == "") {
        # Open the environment file for the user automatically
        usethis::edit_r_environ()

        cli::cli_abort(
            c(
                "No token exists under environment variable {.val {kalshi_access_token}}.",
                "i" = "Your {.file .Renviron} file has been opened.",
                "*" = "Please add {.code {kalshi_access_token}=your_token_here}, save it, and restart R."
            )
        )
    }

    return(out)

 }
 #' Get Kalshi Series Information
 #'
 #' @description
 #' Retrieves a complete list of all recurring event series from the Kalshi API.
 #' This function handles cursor-based pagination automatically, ensuring all
 #' available series are returned in a single flattened table.
 #'
 #' @param kalshi_access_token Character. The name of the environment variable
 #'   storing your Kalshi API token (e.g., from `.Renviron`). Defaults to `"KALSHI_API"`.
 #' @param demo Logical. If `TRUE` (default), uses the demo environment URL.
 #'   If `FALSE`, uses the production trading API URL.
 #'
 #' @details
 #' A "Series" in Kalshi represents a template for recurring events (e.g., "Daily
 #' High Temperature in NYC"). This function transforms the nested JSON response
 #' into a clean, rectangular format.
 #'
 #' Because the Kalshi API limits the number of items per request (default 100),
 #' this function uses a `repeat` loop to follow the `cursor` provided by the
 #' server until all pages are exhausted.
 #'
 #' @return A [tibble::tibble()] containing the following columns:
 #' \itemize{
 #'   \item \code{ticker}: The unique identifier for the series.
 #'   \item \code{title}: The human-readable name of the series.
 #'   \item \code{category}: The market category (e.g., Economics, Politics).
 #'   \item \code{fee_type}: The fee structure applied to trades.
 #'   \item \code{frequency}: How often events in this series occur.
 #'   \item \code{source_name}: The primary settlement data source name.
 #'   \item \code{contract_url}: Link to the specific contract rulebook.
 #' }
 #'
 #' @family market_data
 #' @export
 #'
 #' @examples
 #' \dontrun{
 #' # Fetch all series from the demo environment
 #' series_df <- get_kalshi_series(demo = TRUE)
 #'
 #' # View the first few rows
 #' head(series_df)
 #' }
get_all_events <- function(kalshi_access_token="KALSHI_API"){

     #test
     kalshi_token="KALSHI_API"

     # assign variables
     base_url <- "https://demo-api.kalshi.co"
     path   <- "/trade-api/v2/series"
     method <- "GET"

     all_series_data <- list()
     cursor <- NULL

     cli::cli_progress_step("Fetching series data from Kalshi...")

     # 2. Pagination Loop
     repeat {
         req <- httr2::request(base_url) |>
             httr2::req_url_path(path) |>
             httr2::req_method("GET")

         # Add cursor to query if it exists
         if (!base::is.null(cursor)) {
             req <- req |> httr2::req_url_query(cursor = cursor)
         }

         # Authenticate and Perform
         resp <- req |>
             req_kalshi_auth(
                 key_id = get_kalshi_access_token(kalshi_access_token),
                 private_key_path = .kalshi_env$key_path # Assuming path is in your internal env
             ) |>
             httr2::req_perform() |>
             httr2::resp_body_json()

         all_series_data <- base::append(all_series_data, resp$series)

         # Check if there is another page
         cursor <- resp$cursor
         if (base::is.null(cursor) || cursor == "") break
     }

     # 3. Data Wrangling
     series_tbl <- purrr::map(all_series_data, function(x) {
         # Safe extraction of nested settlement source
         source <- if (base::length(x$settlement_sources) > 0) x$settlement_sources[[1]]$name else NA_character_

         tibble::tibble(
             ticker       = x$ticker    %||% NA_character_,
             title        = x$title     %||% NA_character_,
             category     = x$category  %||% NA_character_,
             fee_type     = x$fee_type  %||% NA_character_,
             frequency    = x$frequency %||% NA_character_,
             source_name  = source,
             contract_url = x$contract_url %||% NA_character_
         )
     }) |>
         purrr::list_rbind()

     cli::cli_alert_success("Retrieved {nrow(series_tbl)} series.")
     return(series_tbl)

 }
