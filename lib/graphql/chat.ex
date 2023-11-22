defmodule Graphql.Chat do
  @moduledoc """
  The Chat context.
  """

  import Ecto.Query, warn: false
  alias Graphql.Repo

  alias Graphql.Chat.Room

  def list_rooms do
    Repo.all(from(r in Room, preload: [:user]))
  end


  def get_room!(id), do: Repo.get!(Room, id)

  def create_room(attrs \\ %{}) do
    %Room{}
    |> Room.changeset(attrs)
    |> Repo.insert()
  end

  def update_room(%Room{} = room, attrs) do
    room
    |> Room.changeset(attrs)
    |> Repo.update()
  end

  def delete_room(%Room{} = room) do
    Repo.delete(room)
  end

  def delete_room_by_id(room_id, user_id) do
    from(r in Room, where: r.id == ^room_id and r.user_id == ^user_id)
    |> Repo.delete_all()
  end

  def change_room(%Room{} = room, attrs \\ %{}) do
    Room.changeset(room, attrs)
  end
end
