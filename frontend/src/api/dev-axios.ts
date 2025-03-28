import axios from "axios";

export const dev = axios.create({
  baseURL: `${window.location.protocol}//${import.meta.env.VITE_BACKEND_BASE_URL || window?.location.host}`,
});
export const setAuthTokenHeader = (token: string) => {
  dev.defaults.headers.common.Authorization = `Bearer ${token}`;
};

export const setGitHubTokenHeader = (token: string) => {
  dev.defaults.headers.common["X-GitHub-Token"] = token;
};

export const removeAuthTokenHeader = () => {
  if (dev.defaults.headers.common.Authorization) {
    delete dev.defaults.headers.common.Authorization;
  }
};

export const removeGitHubTokenHeader = () => {
  if (dev.defaults.headers.common["X-GitHub-Token"]) {
    delete dev.defaults.headers.common["X-GitHub-Token"];
  }
};
