# Internal environment for storing authentication state
.kalshi_env <- new.env(parent = emptyenv())
.kalshi_env$base_url <- "https://api.elections.kalshi.com"
.kalshi_env$demo_url <- "https://demo-api.elections.kalshi.com"

#' Set up Kalshi Authentication
#'
#' @description
#' Configures the session by pointing the package to your private RSA key.
#' This path is stored in an internal environment and used automatically
#' for all subsequent API requests.
#'
#' @param path Character. The absolute or relative path to your `.key` or `.pem` file.
#'
#' @return Invisibly returns the normalized path.
#' @export
#'
#' @examples
#' \dontrun{
#' kalshi_auth_setup("path/to/your/kalshi-key.key")
#' }
kalshi_auth_setup <- function(path) {
    if (!file.exists(path)) {
        cli::cli_abort("The private key file was not found at: {.file {path}}")
    }

    .kalshi_env$key_path <- normalizePath(path)
    cli::cli_alert_success("Kalshi key path set successfully.")

    invisible(.kalshi_env$key_path)
}

#' Validate Kalshi Authentication Setup
#'
#' @description
#' Internal check to ensure the user has run `kalshi_auth_setup()`
#' before attempting an authenticated API call.
#'
#' @return Returns the key path if found; otherwise, throws an error.
#' @keywords internal
check_kalshi_auth <- function() {
    path <- .kalshi_env$key_path

    if (is.null(path)) {
        cli::cli_abort(
            c(
                "Kalshi authentication has not been initialized.",
                "i" = "You must provide the path to your private RSA key.",
                "x" = "Please run: {.fn kalshi_auth_setup}."
            )
        )
    }

    if (!file.exists(path)) {
        cli::cli_abort(
            c(
                "The registered Kalshi key path no longer exists: {.file {path}}",
                "i" = "Please run {.fn kalshi_auth_setup} again with a valid path."
            )
        )
    }

    path
}

#' Generate an RSA Key Pair for Kalshi API Authentication
#'
#' @description
#' Generates a 2048-bit RSA key pair required for Kalshi's V2 API authentication.
#' Saves the private key to a local file and prints the public key to the console
#' for uploading to the Kalshi Developer Dashboard.
#'
#' @param private_key_path Character. The file path where the private key should
#'   be saved. Defaults to `"kalshi-key.key"`.
#'
#' @details
#' Kalshi uses RSA-PSS signing for API requests. To use the API, you must:
#' 1. Generate a key pair using this function.
#' 2. Keep the `.key` file secure (add it to your `.gitignore`).
#' 3. Copy the public key text printed in the console.
#' 4. Log into the Kalshi Dashboard and upload the public key to receive your Key ID.
#'
#' @return Returns the public key PEM string invisibly.
#'
#' @family authentication
#' @export
#'
#' @examples
#' \dontrun{
#' generate_kalshi_keypair("my_kalshi_key.key")
#' }
generate_kalshi_keypair <- function(private_key_path = "kalshi-key.key") {
    key <- openssl::rsa_keygen(bits = 2048)
    openssl::write_pem(key, path = private_key_path)

    pubkey <- as.list(key)$pubkey
    pubkey_pem <- openssl::write_pem(pubkey)

    cli::cli_alert_success("Private key saved to: {.file {private_key_path}}")
    cli::cli_alert_info("--- COPY THE PUBLIC KEY BELOW TO KALSHI DASHBOARD ---")
    cat(pubkey_pem)

    invisible(pubkey_pem)
}

#' Check for OpenSSL System Dependency
#'
#' @description
#' Verifies that the `openssl` command-line tool is installed and accessible
#' via the system's PATH. Required for RSA-PSS signing.
#'
#' @return Returns `TRUE` invisibly if found; otherwise, throws an error.
#'
#' @family authentication
#' @export
#'
#' @examples
#' \dontrun{
#' check_openssl_installed()
#' }
check_openssl_installed <- function() {
    if (Sys.which("openssl") == "") {
        cli::cli_abort(
            c(
                "The {.pkg openssl} command-line tool is required but was not found.",
                "i" = "Install it via your system package manager.",
                "*" = "Ubuntu/Debian: {.code sudo apt install openssl}",
                "*" = "macOS: {.code brew install openssl}",
                "*" = "Windows: Install from https://slproweb.com/products/Win32OpenSSL.html"
            )
        )
    }

    cli::cli_alert_success("{.pkg openssl} command-line tool found.")
    invisible(TRUE)
}

#' Retrieve Kalshi Access Token from Environment
#'
#' @description
#' Retrieves a stored Kalshi API Key ID from the system environment.
#' If not found, prompts the user to edit their `.Renviron` file.
#'
#' @param kalshi_access_token Character. The name of the environment variable
#'   where your Kalshi API Key ID is stored. Defaults to `"KALSHI_API"`.
#'
#' @details
#' Storing API keys in your `.Renviron` file is a best practice to avoid
#' hard-coding secrets into your scripts.
#'
#' To set your key:
#' 1. Run `usethis::edit_r_environ()`
#' 2. Add a line: `KALSHI_API=your_key_id_here`
#' 3. Save and restart R.
#'
#' @return Returns the API Key ID string.
#'
#' @family authentication
#' @export
#'
#' @examples
#' \dontrun{
#' key_id <- get_kalshi_access_token()
#' key_id <- get_kalshi_access_token("MY_PROD_KEY")
#' }
get_kalshi_access_token <- function(kalshi_access_token = "KALSHI_API") {
    if (!is.character(kalshi_access_token)) {
        cli::cli_abort(
            "Kalshi access token name must be a character, not {.cls {class(kalshi_access_token)}}"
        )
    }

    out <- Sys.getenv(kalshi_access_token)

    if (out == "") {
        usethis::edit_r_environ()
        cli::cli_abort(
            c(
                "No token found in environment variable {.val {kalshi_access_token}}.",
                "i" = "Your {.file .Renviron} file has been opened.",
                "*" = "Add {.code {kalshi_access_token}=your_key_id_here}, save, and restart R."
            )
        )
    }

    out
}

#' Internal RSA-PSS Signing via OpenSSL System Call
#'
#' @description
#' Performs RSA-PSS signing using the system's `openssl` binary to generate
#' the `KALSHI-ACCESS-SIGNATURE` header.
#'
#' @param timestamp Character. A millisecond Unix timestamp string.
#' @param method Character. The HTTP method in uppercase (e.g., "GET", "POST").
#' @param path Character. The API endpoint path without query parameters.
#'
#' @details
#' The R `openssl` package does not expose RSA-PSS padding, so this function
#' interfaces directly with the system's `openssl` installation.
#'
#' Signature is generated using:
#' - Message: `timestamp + method + path`
#' - Algorithm: SHA-256
#' - Padding: RSA-PSS
#' - Salt Length: Equal to digest length (32 bytes)
#'
#' @return A base64-encoded string containing the RSA-PSS signature.
#'
#' @keywords internal
sign_kalshi_request <- function(timestamp, method, path) {
    key_path <- check_kalshi_auth()
    msg <- paste0(timestamp, method, path)

    msg_file <- tempfile()
    sig_file <- tempfile()
    on.exit(unlink(c(msg_file, sig_file)), add = TRUE)

    writeBin(charToRaw(msg), con = msg_file)

    args <- c(
        "dgst",
        "-sha256",
        "-sigopt", "rsa_padding_mode:pss",
        "-sigopt", "rsa_pss_saltlen:digest",
        "-sigopt", "rsa_mgf1_md:sha256",
        "-sign", key_path,
        "-out", sig_file,
        msg_file
    )

    result <- system2("openssl", args, stdout = FALSE, stderr = FALSE)

    if (result != 0) {
        cli::cli_abort(
            c(
                "OpenSSL signing failed.",
                "i" = "Ensure {.pkg openssl} is installed and the key path is correct.",
                "x" = "Run {.fn check_openssl_installed} to verify."
            )
        )
    }

    sig_raw <- readBin(sig_file, what = "raw", n = 512)
    openssl::base64_encode(sig_raw)
}

#' Authenticate a Kalshi API Request
#'
#' @description
#' Implements the Kalshi V2 API authentication protocol. Extracts metadata
#' from an `httr2` request, generates a SHA-256 RSA-PSS signature, and
#' attaches the required headers.
#'
#' @param req An `httr2` request object.
#' @param kalshi_access_token Character. The name of the environment variable
#'   containing your Kalshi Key ID. Defaults to `"KALSHI_API"`.
#'
#' @details
#' The function signs the base path without query parameters, as required by
#' Kalshi's authentication scheme.
#'
#' @return A modified `httr2` request object with authentication headers.
#' @export
req_kalshi_auth <- function(req, kalshi_access_token = "KALSHI_API") {
    check_kalshi_auth()

    timestamp_ms <- as.character(round(as.numeric(Sys.time()) * 1000))

    parsed_url <- httr2::url_parse(req$url)
    path_to_sign <- parsed_url$path

    if (!grepl("^/", path_to_sign)) {
        path_to_sign <- paste0("/", path_to_sign)
    }

    method <- req$method %||% "GET"

    signature <- sign_kalshi_request(
        timestamp = timestamp_ms,
        method = method,
        path = path_to_sign
    )

    req |>
        httr2::req_headers(
            `KALSHI-ACCESS-KEY` = get_kalshi_access_token(kalshi_access_token),
            `KALSHI-ACCESS-SIGNATURE` = signature,
            `KALSHI-ACCESS-TIMESTAMP` = timestamp_ms
        )
}

#' Get Kalshi Base URL
#'
#' @description
#' Returns the appropriate base URL for API requests.
#'
#' @param demo Logical. If `TRUE`, returns the demo environment URL.
#'   If `FALSE` (default), returns the production URL.
#'
#' @return Character string with the base URL.
#' @keywords internal
kalshi_base_url <- function(demo = FALSE) {
    if (demo) {
        .kalshi_env$demo_url
    } else {
        .kalshi_env$base_url
    }
}
