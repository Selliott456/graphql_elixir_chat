defmodule GraphqlWeb.AuthView do
use GraphqlWeb, :view

def render("acknowledge.json", %{message: message}) do
  %{success: true, message: message}
end

def render("errors.json", %{errors:  errors}) do
  %{success: false, errors: errors}
end
end
