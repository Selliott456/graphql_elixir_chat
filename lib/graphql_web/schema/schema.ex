defmodule GraphqlWeb.Schema do
  use Absinthe.Schema
  # alias GraphqlWeb.Schema.Resolvers.RoomResolver
  alias GraphqlWeb.Schema.Resolvers
  alias GraphqlWeb.Topics

  import_types(GraphqlWeb.Schema.Types.RoomType)
  import_types(GraphqlWeb.Schema.Types.UserType)
  import_types(GraphqlWeb.Schema.Types.MessageType)

  @desc "greet"
  query do
    field :hello, :string do
      resolve(fn _, _, _ -> {:ok, "worlds"} end)
    end

  @desc "Get all users"
    field :users, list_of(:user_type) do
      resolve(&Resolvers.UserResolver.get_all_users/3)
    end

  @desc "Get Me"
    field :get_me, :user_type do
      resolve(&Resolvers.UserResolver.get_me/3)
    end

  @desc "Get all rooms"
    field :rooms, list_of(:room_type) do
    resolve(&Resolvers.RoomResolver.get_all_rooms/3)
    end

  @desc "get all messages"
    field :messages, list_of(:message_type) do
      arg(:input, non_null(:list_messages_type))
      resolve(&Resolvers.MessageResolver.get_all_messages/3)
    end

  end

  mutation do
    @desc "Create room"
    field :create_room, :boolean do
      arg(:input, non_null(:room_input_type))
      resolve(&Resolvers.RoomResolver.create_room/3)
    end

    @desc "Delete room"
    field :delete_room, :boolean do
      arg(:input, non_null(:delete_room_input))
      resolve(&Resolvers.RoomResolver.delete_room/3)
    end

    @desc "Create message"
    field :create_message, :message_type do
      arg(:input, non_null(:message_input_type))
      resolve(&Resolvers.MessageResolver.create_message/3)
    end

    @desc "Delete message"
    field :delete_message, :boolean do
      arg(:input, non_null(:delete_message_input))
      resolve(&Resolvers.MessageResolver.delete_message/3)
    end
  end

  subscription do

    @desc "New message"
    field :new_message, :message_type do
      arg(:input, non_null(:delete_room_input))

      config(fn %{input: input}, _ ->
        IO.inspect(input, label: "INPUT NEW MESSAGE")
        {:ok, topic: "#{input.room_id}: #{Topics.Topics.new_message()}"}
      end)

      trigger(:create_message, topic: fn new_message ->
        IO.inspect(new_message)
        "#{new_message.room_id}:#{Topics.Topics.new_message()}"
      end)

      resolve(fn new_message, _, _ ->
        {:ok, new_message}
      end)
    end
  end
end
