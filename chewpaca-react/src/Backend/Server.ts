export let serverlocation = window.location.origin;

export function getServerLocation(): string {
  switch (window.location.hostname) {
    case "localhost":
      serverlocation = "http://localhost:8080";
      break;

    default:
      break;
  }

  return serverlocation;
}

getServerLocation();
