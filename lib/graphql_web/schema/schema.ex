defmodule GraphqlWeb.Schema do
  use Absinthe.Schema
  alias GraphqlWeb.Schema.Resolvers

  import_types(GraphqlWeb.Schema.Types.Types)

  @desc "greet"
  query do
    field :hello, :string do
      resolve(fn _, _, _ -> {:ok, "worlds"} end)
    end
  @dec "Get all users"
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
  end
end
