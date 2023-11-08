defmodule GraphqlWeb.Schema.Resolvers.UserResolver do

  alias Graphql.Auth



  def get_all_users(_, _, _) do
    {:ok, Auth.list_users()}
  end
end
