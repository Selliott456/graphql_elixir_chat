defmodule GraphqlWeb.AuthController do
  use GraphqlWeb, :controller

  def index(conn, _params) do
    render(conn, "acknowledge.json", %{message: "hello"})
  end
end
