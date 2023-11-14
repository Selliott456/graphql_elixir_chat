defmodule Graphql.ChatFixtures do
  @moduledoc """
  This module defines test helpers for creating
  entities via the `Graphql.Chat` context.
  """

  @doc """
  Generate a unique room name.
  """
  def unique_room_name, do: "some name#{System.unique_integer([:positive])}"

  @doc """
  Generate a room.
  """
  def room_fixture(attrs \\ %{}) do
    {:ok, room} =
      attrs
      |> Enum.into(%{
        name: unique_room_name(),
        description: "some description"
      })
      |> Graphql.Chat.create_room()

    room
  end
end
