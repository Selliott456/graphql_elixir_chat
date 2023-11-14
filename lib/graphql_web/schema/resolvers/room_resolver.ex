defmodule GraphqlWeb.Schema.Resolvers.RoomResolver do
  alias Graphql.Chat.Room

  def get_all_rooms(_, _, %{context: context}) do
    {:ok, Auth.list_rooms()}
  end
end
