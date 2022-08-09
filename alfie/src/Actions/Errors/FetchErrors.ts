export class UnauthorizedError extends Error {
  constructor() {
    super("UnauthorizedError");
    this.name = "UnauthorizedError";
  }
}

export class Non200Error extends Error {
  statusCode: number;
  url: string;

  constructor(res: Response) {
    super(
      `Error code ${res.status} detected, can you let us know at hi@paytgthr.com?`
    );
    this.name = "Non200Error";
    this.statusCode = res.status;
    this.url = res.url;
  }
}
