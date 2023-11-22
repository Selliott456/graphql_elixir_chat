defmodule GraphqlWeb.Schema.Resolvers.RoomResolver do
  alias Graphql.Chat
  alias GraphqlWeb.Utils.Utils
  alias GraphqlWeb.Constants.Constants

  def get_all_rooms(_, _, %{context: _context}) do
    rooms = Chat.list_rooms()
    {:ok, rooms}
  end

  def create_room(_, %{input: input}, %{context: context}) do
    input_with_user_id = Map.merge(input, %{user_id: context.current_user.id})

    case Chat.create_room(input_with_user_id) do
      {:ok, _room} -> {:ok, true}
      {:error, %Ecto.Changeset{} = changeset} -> {:error, Utils.format_changeset_errors(changeset)}
      _ -> {:error, Constants.internal_server_error}
    end


  end
end
