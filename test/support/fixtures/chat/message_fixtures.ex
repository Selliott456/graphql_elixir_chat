defmodule Graphql.Chat.MessageFixtures do
  @moduledoc """
  This module defines test helpers for creating
  entities via the `Graphql.Chat.Message` context.
  """

  @doc """
  Generate a message.
  """
  def message_fixture(attrs \\ %{}) do
    {:ok, message} =
      attrs
      |> Enum.into(%{

      })
      |> Graphql.Chat.Message.create_message()

    message
  end
end
