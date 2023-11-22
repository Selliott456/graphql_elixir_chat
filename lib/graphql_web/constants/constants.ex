defmodule GraphqlWeb.Constants.Constants do
  @internal_server_error "Internal Server Error"
  @invalid_credentials "Invalid username or password"
  @not_authorized "Not authorized"
  @not_authenticated "You must log in to access this page"
  @not_found "Not found"

  def internal_server_error, do: @internal_server_error
  def invalid_credentials, do: @invalid_credentials
  def not_authorized, do: @not_authorized
  def not_authenticated, do: @not_authenticated
  def not_found, do: @not_found
end
