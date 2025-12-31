
devtools::load_all()
devtools::document()


kalshi::check_openssl_installed()

kalshi_auth_setup()

kalshi::get_kalshi_access_token()

kalshi_auth_setup()

all_events_tbl <- kalshi::get_all_events()



all_events_tbl |> count(category)


