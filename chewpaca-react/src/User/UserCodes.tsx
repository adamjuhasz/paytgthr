import useGetCodes from "./UseGetCodes";

interface Props {
  user: string;
}

export default function UserCodes(props: Props): JSX.Element {
  const codes = useGetCodes(props.user);

  if (codes.status === "success") {
    const active = codes.data.filter(
      (c) => new Date(c.expiresat).valueOf() >= new Date().valueOf()
    );
    const expired = codes.data.filter(
      (c) => new Date(c.expiresat).valueOf() < new Date().valueOf()
    );
    return (
      <div>
        <div className="bg-white overflow-hidden shadow rounded-lg font-normal mb-8">
          <div className="px-4 py-5 sm:p-6">
            <form
              className="inline"
              method="post"
              action={`/user/${props.user}/token/create/email`}
            >
              <span className="inline-flex rounded-md shadow-sm">
                <button
                  type="submit"
                  className=" inline-flex items-center px-4 py-2 border border-transparent text-sm leading-5 font-medium rounded-md focus:outline-none transition ease-in-out duration-150  text-white bg-indigo-600 hover:bg-indigo-500 focus:border-indigo-700 focus:shadow-outline-indigo active:bg-indigo-700 "
                >
                  Send Email Token
                </button>
              </span>
            </form>
            <form
              className="inline"
              method="post"
              action={`/user/${props.user}/token/create/phone`}
            >
              <span className="inline-flex rounded-md shadow-sm ml-1">
                <button
                  type="submit"
                  className=" inline-flex items-center px-4 py-2 border border-transparent text-sm leading-5 font-medium rounded-md focus:outline-none transition ease-in-out duration-150  text-white bg-indigo-600 hover:bg-indigo-500 focus:border-indigo-700 focus:shadow-outline-indigo active:bg-indigo-700 "
                >
                  Send Phone Token
                </button>
              </span>
            </form>
            <form
              className="inline"
              method="post"
              action={`/user/${props.user}/token/create/push`}
            >
              <span className="inline-flex rounded-md shadow-sm ml-1">
                <button
                  type="submit"
                  className=" inline-flex items-center px-4 py-2 border border-transparent text-sm leading-5 font-medium rounded-md focus:outline-none transition ease-in-out duration-150  text-white bg-indigo-600 hover:bg-indigo-500 focus:border-indigo-700 focus:shadow-outline-indigo active:bg-indigo-700 "
                >
                  Send Push Token
                </button>
              </span>
            </form>
          </div>
          <form
            className="block"
            method="post"
            action={`/user/${props.user}/token/verify/phone`}
          >
            <div className="bg-white overflow-hidden rounded-lg">
              <div className="px-4 py-5 sm:p-6">
                <div className="mt-1 relative rounded-md ">
                  <input
                    name="token"
                    id="tokenInput"
                    placeholder="123456"
                    type="decimal"
                    className="form-input sm:text-sm sm:leading-5 shadow-sm"
                  />
                  <span className="inline-flex rounded-md shadow-sm ml-1">
                    <button
                      type="submit"
                      className=" inline-flex items-center px-4 py-2 border border-transparent text-sm leading-5 font-medium rounded-md focus:outline-none transition ease-in-out duration-150  text-white bg-indigo-600 hover:bg-indigo-500 focus:border-indigo-700 focus:shadow-outline-indigo active:bg-indigo-700 "
                    >
                      Verify Token
                    </button>
                  </span>
                </div>
              </div>
            </div>
          </form>
        </div>
        <h2 className="text-gray-500 text-xs font-medium uppercase tracking-wide">
          Active
        </h2>
        <ul
          className="mt-3 grid grid-cols-1 gap-5 sm:gap-6 sm:grid-cols-2 lg:grid-cols-4"
          style={{ minHeight: "3rem" }}
        >
          {active.map((code) => {
            let bgColor = "bg-pink-600";
            let text = "U";
            switch (code.token.tag) {
              case "EmailToken":
                bgColor = "bg-purple-600";
                text = "Email";
                break;
              case "PhoneToken":
                bgColor = "bg-green-500";
                text = "Phone";
                break;
            }

            return (
              <li
                key={code.createdon}
                className="col-span-1 flex shadow-sm rounded-md"
              >
                <div
                  className={`${bgColor} flex-shrink-0 flex items-center justify-center w-16 text-white text-sm font-medium rounded-l-md`}
                >
                  {text}
                </div>
                <div className="flex-1 flex items-center justify-between border-t border-r border-b border-gray-200 bg-white rounded-r-md truncate">
                  <div className="flex-1 px-4 py-2 text-sm truncate">
                    {code.token.contents[0]}
                    <p className="text-gray-500">{code.token.contents[1]}</p>
                  </div>
                  <div className="flex-shrink-0 pr-2"></div>
                </div>
              </li>
            );
          })}
        </ul>
        <div className="h-4" />
        <h2 className="text-gray-500 text-xs font-medium uppercase tracking-wide">
          Expired
        </h2>
        <ul className="mt-3 grid grid-cols-1 gap-5 sm:gap-6 sm:grid-cols-2 lg:grid-cols-4">
          {expired.map((code) => {
            let bgColor = "bg-pink-600";
            let text = "U";
            switch (code.token.tag) {
              case "EmailToken":
                bgColor = "bg-purple-600";
                text = "E";
                break;
              case "PhoneToken":
                bgColor = "bg-green-500";
                text = "P";
                break;
            }

            return (
              <li
                key={code.createdon}
                className="col-span-1 flex shadow-sm rounded-md"
              >
                <div
                  className={`${bgColor} flex-shrink-0 flex items-center justify-center w-16 text-white text-sm font-medium rounded-l-md`}
                >
                  {text}
                </div>
                <div className="flex-1 flex items-center justify-between border-t border-r border-b border-gray-200 bg-white rounded-r-md truncate">
                  <div className="flex-1 px-4 py-2 text-sm truncate">
                    {code.token.contents[1]}
                    <p className="text-gray-500">{code.token.contents[0]}</p>
                    <p className="text-gray-500">
                      Expired: {new Date(code.expiresat).toLocaleString()}
                    </p>
                    <p className="text-gray-500">
                      Created: {new Date(code.createdon).toLocaleString()}
                    </p>
                  </div>
                  <div className="flex-shrink-0 pr-2"></div>
                </div>
              </li>
            );
          })}
        </ul>
      </div>
    );
  }

  return <div>Loading codes</div>;
}
