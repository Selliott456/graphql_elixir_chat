defmodule GraphqlWeb.Router do
  alias GraphqlWeb.ProtectGraphql
  alias GraphqlWeb.AuthController
  alias GraphqlWeb.PopulateAuth
  alias GraphqlWeb.Plugs.ProtectGraphql
  use GraphqlWeb, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_live_flash
    plug :put_root_layout, {GraphqlWeb.LayoutView, :root}
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :api do
    plug :accepts, ["json"]
    plug :fetch_session
    plug PopulateAuth
  end

  pipeline :graphql do
    plug :accepts, ["json"]
    plug :fetch_session
    plug ProtectGraphql
  end

  scope "/api" do
    pipe_through :api
    post "/auth/register", AuthController, :register
    post "/auth/login", AuthController, :login
    delete "/auth/logout", AuthController, :logout
    get "/auth/getme", AuthController, :getme
  end

  scope "/api/graphql" do
    pipe_through :graphql

    get "/", Absinthe.Plug.GraphiQL, schema: GraphqlWeb.Schema,
    interface: :playground, socket: GraphqlWeb.Socket
    post "/", Absinthe.Plug, schema: GraphqlWeb.Schema
  end

  # Other scopes may use custom stacks.
  # scope "/api", GraphqlWeb do
  #   pipe_through :api
  # end

  # Enables LiveDashboard only for development
  #
  # If you want to use the LiveDashboard in production, you should put
  # it behind authentication and allow only admins to access it.
  # If your application does not have an admins-only section yet,
  # you can use Plug.BasicAuth to set up some basic authentication
  # as long as you are also using SSL (which you should anyway).
  if Mix.env() in [:dev, :test] do
    import Phoenix.LiveDashboard.Router

    scope "/" do
      pipe_through :browser

      live_dashboard "/dashboard", metrics: GraphqlWeb.Telemetry
    end
  end

  # Enables the Swoosh mailbox preview in development.
  #
  # Note that preview only shows emails that were sent by the same
  # node running the Phoenix server.
  if Mix.env() == :dev do
    scope "/dev" do
      pipe_through :browser

      forward "/mailbox", Plug.Swoosh.MailboxPreview
    end
  end
end
