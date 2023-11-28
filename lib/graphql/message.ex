defmodule Graphql.Message do
    @moduledoc """
    The Message context.
    """
    import Ecto.Query, warn: false
    alias Graphql.Repo
    alias Graphql.Chat.Message


    def list_messages do
      IO.puts("list messages func")
      Repo.all(from(r in Message, preload: [:user]))
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

    def delete_message_by_id(message_id, user_id) do
      from(r in Message, where: r.id == ^message_id and r.user_id == ^user_id)
      |> Repo.delete_all()
    end


    def change_message(%Message{} = message, attrs \\ %{}) do
      Message.changeset(message, attrs)
    end
end
