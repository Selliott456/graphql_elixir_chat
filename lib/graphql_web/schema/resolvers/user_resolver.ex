defmodule GraphqlWeb.Schema.Resolvers.UserResolver do

  alias GraphqlWeb.Constants.Constants
  alias Graphql.Auth
  alias GraphqlWeb.Utils.Utils
  alias GraphqlWeb.Constants.Constants

  def register_user(_, %{input: input}, _) do
    case Auth.create_user(input) do
      {:ok, _} -> {:ok, true}
      {:error, %Ecto.Changeset{} = changeset} ->
        errors = Utils.format_changeset_errors(changeset)
        {:error, errors}
      _ -> {:error, Constants.internal_server_error()}
    end

  end

  def get_all_users(_, _, _) do
    {:ok, Auth.list_users()}
  end
end
