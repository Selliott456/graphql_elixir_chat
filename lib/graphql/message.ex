defmodule Graphql.Message do
    @moduledoc """
    The Message context.
    """
    import Ecto.Query, warn: false
    alias Graphql.Repo
    alias Graphql.Chat.Message


    def list_messages(room_id, cursor \\ nil ) do

      limit = 10

      case cursor do
        nil -> Repo.all(from(m in Message, where: m.room_id == ^room_id, limit: ^limit, preload: [:user, :room]))

        cursor -> Repo.all(from(m in Message, where: m.room_id == ^room_id and m.id > ^cursor, limit: ^limit, preload: [:user, :room]))
      end
    end

    def get_message!(id), do: Repo.get!(Message, id)

    def create_message(attrs \\ %{}) do
      %Message{}
      |> Message.changeset(attrs)
      |> Repo.insert()
    end

    def update_message(%Message{} = message, attrs) do
      message
      |> Message.changeset(attrs)
      |> Repo.update()
    end

    def delete_message(%Message{} = message) do
      Repo.delete(message)
    end

    def change_message(%Message{} = message, attrs \\ %{}) do
      Message.changeset(message, attrs)
    end

    def delete_message_by_id(message_id, room_id, user_id) do
      from(m in Message, where: m.id == ^message_id and m.user_id == ^user_id and m.room_id == ^room_id)
      |> Repo.delete_all()
    end

end
