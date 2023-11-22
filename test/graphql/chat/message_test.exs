defmodule Graphql.Chat.MessageTest do
  use Graphql.DataCase

  alias Graphql.Chat.Message

  describe "messages" do
    alias Graphql.Chat.Message.Message

    import Graphql.Chat.MessageFixtures

    @invalid_attrs %{}

    test "list_messages/0 returns all messages" do
      message = message_fixture()
      assert Message.list_messages() == [message]
    end

    test "get_message!/1 returns the message with given id" do
      message = message_fixture()
      assert Message.get_message!(message.id) == message
    end

    test "create_message/1 with valid data creates a message" do
      valid_attrs = %{}

      assert {:ok, %Message{} = message} = Message.create_message(valid_attrs)
    end

    test "create_message/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = Message.create_message(@invalid_attrs)
    end

    test "update_message/2 with valid data updates the message" do
      message = message_fixture()
      update_attrs = %{}

      assert {:ok, %Message{} = message} = Message.update_message(message, update_attrs)
    end

    test "update_message/2 with invalid data returns error changeset" do
      message = message_fixture()
      assert {:error, %Ecto.Changeset{}} = Message.update_message(message, @invalid_attrs)
      assert message == Message.get_message!(message.id)
    end

    test "delete_message/1 deletes the message" do
      message = message_fixture()
      assert {:ok, %Message{}} = Message.delete_message(message)
      assert_raise Ecto.NoResultsError, fn -> Message.get_message!(message.id) end
    end

    test "change_message/1 returns a message changeset" do
      message = message_fixture()
      assert %Ecto.Changeset{} = Message.change_message(message)
    end
  end
end
