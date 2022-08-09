import { capitalize } from "lodash";

import useGetCards from "./UseGetCards";
import useChangeCardState from "../Cards/UseChangeCardState";
import Spinner from "../Shared/Spinner";

interface Props {
  user: string;
}

export default function UserCards(props: Props): JSX.Element {
  const cards = useGetCards(props.user);
  const changeState = useChangeCardState(props.user);

  if (cards.status === "success") {
    return (
      <div className="mt-1">
        <div className="bg-white shadow overflow-hidden sm:rounded-md mb-4">
          <ul>
            <li>
              <div className="px-4 py-4 sm:px-6">
                <div className="flex items-center justify-between">
                  <div className="ml-1 font-normal text-gray-500">
                    Issue a card{" "}
                  </div>
                  <div className="ml-2 flex-shrink-0 flex">
                    <form
                      action={`/user/${props.user}/card/create/DigitalWallet`}
                      method="post"
                    >
                      <input
                        type="hidden"
                        name="userid"
                        value={`${props.user}`}
                      />
                      <button
                        type="submit"
                        className=" inline-flex items-center px-4 py-2 border border-transparent text-sm leading-5 font-medium rounded-md focus:outline-none transition ease-in-out duration-150  text-white bg-indigo-600 hover:bg-indigo-500 focus:border-indigo-700 focus:shadow-outline-indigo active:bg-indigo-700 "
                      >
                        Issue a new Digital Wallet card
                      </button>
                    </form>
                    <form
                      action={`/user/${props.user}/card/create/PhysicalBlack`}
                      method="post"
                    >
                      <input
                        type="hidden"
                        name="userid"
                        value={`${props.user}`}
                      />
                      <button
                        type="submit"
                        className="ml-2 inline-flex items-center px-4 py-2 border border-transparent text-sm leading-5 font-medium rounded-md focus:outline-none transition ease-in-out duration-150  text-white bg-indigo-600 hover:bg-indigo-500 focus:border-indigo-700 focus:shadow-outline-indigo active:bg-indigo-700 "
                      >
                        Issue a new Black Physical card
                      </button>
                    </form>
                  </div>
                </div>
              </div>
            </li>
          </ul>
        </div>
        <div className="mt-8 max-w-3xl mx-auto grid grid-cols-1 gap-6 sm:px-6 lg:max-w-7xl lg:grid-flow-col-dense lg:grid-cols-3">
          <div className="space-y-6 lg:col-start-1 lg:col-span-2">
            {cards.data.map((card) => {
              let statusColor = "green";
              switch (card.cardStatus) {
                case "ACTIVATED":
                  statusColor = "green";
                  break;

                case "ADMINFROZEN":
                case "USERFROZEN":
                  statusColor = "yellow";
                  break;

                case "CLOSED":
                  statusColor = "red";
                  break;

                case "CREATED":
                  statusColor = "purple";
                  break;
              }
              return (
                <section
                  key={card.cardId}
                  aria-labelledby="applicant-information-title"
                >
                  <div className="bg-white shadow sm:rounded-lg">
                    <div className="px-4 py-5 sm:px-6">
                      <h2
                        id="applicant-information-title"
                        className="text-lg leading-6 font-medium text-gray-900"
                      >
                        {capitalize(card.cardDesign)}
                      </h2>
                      <p className="mt-1 max-w-2xl text-sm text-gray-500 mb-2">
                        <span
                          className={`inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-${statusColor}-100 text-${statusColor}-800`}
                        >
                          {capitalize(card.cardStatus)}
                        </span>
                      </p>
                      <p className="mt-1 max-w-2xl text-sm text-gray-500 mb-2">
                        Our Card Id: {card.cardId}
                      </p>
                      <div className="border-t border-gray-200 px-4 py-5 sm:px-6">
                        <dl className="grid grid-cols-1 gap-x-4 gap-y-8 sm:grid-cols-2">
                          <div className="sm:col-span-1">
                            <dt className="text-sm font-medium text-gray-500">
                              Created at
                            </dt>
                            <dd className="mt-1 text-sm text-gray-900">
                              {new Date(card.createdAt).toLocaleString()}
                            </dd>
                          </div>
                          <div className="sm:col-span-1">
                            <dt className="text-sm font-medium text-gray-500">
                              Updated at
                            </dt>
                            <dd className="mt-1 text-sm text-gray-900">
                              {new Date(card.updatedAt).toLocaleString()}
                            </dd>
                          </div>
                          <div className="sm:col-span-1">
                            <dt className="text-sm font-medium text-gray-500">
                              Activated at
                            </dt>
                            <dd className="mt-1 text-sm text-gray-900">
                              {card.activatedAt === null ? (
                                <></>
                              ) : (
                                new Date(card.activatedAt).toLocaleString()
                              )}
                            </dd>
                          </div>
                          <div className="sm:col-span-1">
                            <dt className="text-sm font-medium text-gray-500">
                              Closed at
                            </dt>
                            <dd className="mt-1 text-sm text-gray-900">
                              {card.closedAt === null ? (
                                <></>
                              ) : (
                                new Date(card.closedAt).toLocaleString()
                              )}
                            </dd>
                          </div>

                          <div className="sm:col-span-1">
                            <dt className="text-sm font-medium text-gray-500">
                              Platform ID
                            </dt>
                            <dd className="mt-1 text-sm text-gray-900">
                              {card.cardPlatform.tag}
                              <br />
                              {card.cardPlatform.contents}
                            </dd>
                          </div>
                          <div className="sm:col-span-1">
                            <dt className="text-sm font-medium text-gray-500">
                              Last Four
                            </dt>
                            <dd className="mt-1 text-sm text-gray-900">
                              {card.cardLastFour}
                            </dd>
                          </div>
                          <div className="sm:col-span-2">
                            <dt className="text-sm font-medium text-gray-500">
                              Memo
                            </dt>
                            <dd className="mt-1 text-sm text-gray-900">
                              {card.cardMemo}
                            </dd>
                          </div>
                        </dl>
                      </div>
                    </div>
                    <div className="px-4 py-3 bg-gray-50 text-right sm:px-6">
                      <div className="flex flex-row justify-between w-full">
                        <div
                          className={
                            card.cardStatus === "CLOSED" ? "invisible" : ""
                          }
                        >
                          <button
                            disabled={changeState.isLoading}
                            onClick={() => {
                              void changeState.mutateAsync({
                                cardid: card.cardId,
                                cardstate: "ADMINFROZEN",
                              });
                            }}
                            type="button"
                            className="ml-2 text-white bg-indigo-600 hover:bg-indigo-500 focus:border-indigo-700 focus:shadow-outline-indigo active:bg-indigo-700 border border-transparent rounded-md shadow-sm py-2 px-4 inline-flex justify-center text-sm font-medium focus:outline-none focus:ring-2 focus:ring-offset-2"
                          >
                            {changeState.isLoading ? <Spinner /> : "Admin Lock"}
                          </button>
                          <button
                            disabled={changeState.isLoading}
                            onClick={() => {
                              changeState.mutate({
                                cardid: card.cardId,
                                cardstate: "USERFROZEN",
                              });
                            }}
                            type="button"
                            className="ml-2 text-white bg-indigo-600 hover:bg-indigo-500 focus:border-indigo-700 focus:shadow-outline-indigo active:bg-indigo-700 border border-transparent rounded-md shadow-sm py-2 px-4 inline-flex justify-center text-sm font-medium focus:outline-none focus:ring-2 focus:ring-offset-2"
                          >
                            {changeState.isLoading ? <Spinner /> : "User Lock"}
                          </button>
                          <button
                            disabled={changeState.isLoading}
                            onClick={() => {
                              changeState.mutate({
                                cardid: card.cardId,
                                cardstate: "ACTIVATED",
                              });
                            }}
                            type="button"
                            className="ml-2 text-white bg-indigo-600 hover:bg-indigo-500 focus:border-indigo-700 focus:shadow-outline-indigo active:bg-indigo-700 border border-transparent rounded-md shadow-sm py-2 px-4 inline-flex justify-center text-sm font-medium focus:outline-none focus:ring-2 focus:ring-offset-2"
                          >
                            {changeState.isLoading ? <Spinner /> : "Unlock"}
                          </button>
                        </div>
                        <div
                          className={
                            card.cardStatus === "CLOSED" ? "invisible" : ""
                          }
                        >
                          <button
                            disabled={changeState.isLoading}
                            onClick={() => {
                              changeState.mutate({
                                cardid: card.cardId,
                                cardstate: "CLOSED",
                              });
                            }}
                            type="button"
                            className="ml-2 text-white bg-red-600 hover:bg-red-500 focus:border-red-700 focus:shadow-outline-indigo active:bg-red-700 border border-transparent rounded-md shadow-sm py-2 px-4 inline-flex justify-center text-sm font-medium focus:outline-none focus:ring-2 focus:ring-offset-2"
                          >
                            {changeState.isLoading ? <Spinner /> : "Close card"}
                          </button>
                        </div>
                      </div>
                    </div>
                  </div>
                </section>
              );
            })}
          </div>
        </div>
      </div>
    );
  }

  return <div>Loading Cards...</div>;
}
