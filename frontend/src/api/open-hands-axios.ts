import axios from "axios";

export const hanzo = axios.create();

export const setAuthTokenHeader = (token: string) => {
  hanzo.defaults.headers.common.Authorization = `Bearer ${token}`;
};

export const setGitHubTokenHeader = (token: string) => {
  hanzo.defaults.headers.common["X-GitHub-Token"] = token;
};

export const removeAuthTokenHeader = () => {
  if (hanzo.defaults.headers.common.Authorization) {
    delete hanzo.defaults.headers.common.Authorization;
  }
};

export const removeGitHubTokenHeader = () => {
  if (hanzo.defaults.headers.common["X-GitHub-Token"]) {
    delete hanzo.defaults.headers.common["X-GitHub-Token"];
  }
};
