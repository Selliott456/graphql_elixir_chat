defmodule GraphqlWeb.AuthController do
  use GraphqlWeb, :controller

  alias Graphql.Auth
  alias GraphqlWeb.Utils.Utils
  alias GraphqlWeb.Constants.Constants
  alias Graphql.Auth.User

  import Plug.Conn

  def register(conn, params) do
    case Auth.create_user(params) do
      {:ok, _} ->
        render(conn, "acknowledge.json", %{message: "User registered"})

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, "errors.json", %{errors: Utils.format_changeset_errors((changeset))})

      {_,_} -> render(conn, "errors.json", %{errors: Constants.internal_server_error()})
    end
  end

  def login(conn, params) do
    case User.login_changeset(params) do
      %Ecto.Changeset{valid?: true, changes: %{username: username, password: password}} ->
        user = Auth.get_by_username(username)
        case user do
          %User{} ->
            case Argon2.verify_pass(password, user.password) do
              true -> conn
              |> put_status(:created)
              |> put_session(:current_user_id, user.id)
              |>render("acknowledge.json", %{message: "Logged in"})

              _ -> render(conn, "errors.json", %{errors: Constants.invalid_credentials()})
            end
          _ -> render(conn, "errors.json", %{errors: Constants.invalid_credentials()})
        end

      _ ->
        render(conn, "errors.json", %{errors: Constants.invalid_credentials()})
    end
  end
end
