defmodule GraphqlWeb.Schema.Resolvers.UserResolver do

  alias Graphql.Auth

  def get_all_users(_, _, %{context: context}) do
    {:ok, Auth.list_users()}
  end

  def get_me(_, _, %{context: %{current_user: current_user}}) do
    {:ok, current_user}
  end
end
