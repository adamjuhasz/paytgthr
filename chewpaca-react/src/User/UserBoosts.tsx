import { defaultTo } from "lodash";
import { Fragment, useState } from "react";
import { Listbox, Transition } from "@headlessui/react";
import { CheckIcon, SelectorIcon } from "@heroicons/react/solid";

import useGetGroups from "./UseGetGroups";
import useGetBoosts from "./UseGetBoost";
import useGetAllBoosts from "../Rewards/UseGetBoosts";
import useGetBoostActivations from "./UseGetBoostActivations";
import { useActivateBoost } from "./UseChangeBoosts";
import { Group } from "../Group/Types";

import BoostLists from "../Rewards/BoostList";
import ActivationsList from "../Rewards/ActivationsList";
import { RewardBoost } from "../Rewards/Types";

function classNames(...classes: string[]) {
  return classes.filter(Boolean).join(" ");
}

interface Props {
  user: string;
}

export default function UserBoosts({ user }: Props): JSX.Element {
  const group = useGetGroups(user);
  const activeGroup: Group | undefined = defaultTo(group.data, []).filter(
    (g) => g.status === "groupactive"
  )[0];

  const boosts = useGetBoosts(activeGroup?.id || "", activeGroup !== undefined);
  const activations = useGetBoostActivations(
    activeGroup?.id || "",
    activeGroup !== undefined
  );
  const allBoosts = useGetAllBoosts();
  const activateBoost = useActivateBoost({
    userid: user,
    groupid: activeGroup?.id || "",
  });

  const [selected, setSelected] = useState<RewardBoost | null>(null);

  let content = <></>;

  if (group.status === "success" && group.data.length === 0) {
    return <div>No active group so no boosts</div>;
  }

  if (boosts.status === "success" && allBoosts.status === "success") {
    content = (
      <>
        {content}
        <div className="bg-white shadow sm:rounded-lg">
          <div className="px-4 py-5 sm:p-6">
            <h3 className="text-lg leading-6 font-medium text-gray-900">
              Activate new boosts
            </h3>

            <div className="flex justify-between">
              <Listbox value={selected} onChange={setSelected}>
                {({ open }) => (
                  <div>
                    <Listbox.Label className="block text-sm font-medium text-gray-700">
                      Activate a new boost for this user
                    </Listbox.Label>
                    <div className="mt-1 relative">
                      <Listbox.Button className="relative w-72 bg-white border border-gray-300 rounded-md shadow-sm pl-3 pr-10 py-2 text-left cursor-default focus:outline-none focus:ring-1 focus:ring-indigo-500 focus:border-indigo-500 sm:text-sm">
                        <div className="flex items-center">
                          <span
                            className={classNames(
                              selected === null
                                ? "bg-transparent"
                                : selected.boostActive
                                ? "bg-green-400"
                                : "bg-red-400",
                              "flex-shrink-0 inline-block h-2 w-2 rounded-full"
                            )}
                          />
                          <span className="ml-3 block truncate">
                            {defaultTo(selected?.boostName, "Choose one")}
                          </span>
                        </div>
                        <span className="absolute inset-y-0 right-0 flex items-center pr-2 pointer-events-none">
                          <SelectorIcon
                            className="h-5 w-5 text-gray-400"
                            aria-hidden="true"
                          />
                        </span>
                      </Listbox.Button>

                      <Transition
                        show={open}
                        as={Fragment}
                        leave="transition ease-in duration-100"
                        leaveFrom="opacity-100"
                        leaveTo="opacity-0"
                      >
                        <Listbox.Options className="absolute z-10 mt-1 w-72 bg-white shadow-lg max-h-60 rounded-md py-1 text-base ring-1 ring-black ring-opacity-5 overflow-auto focus:outline-none sm:text-sm">
                          {allBoosts.data.map((boost) => (
                            <Listbox.Option
                              key={boost.boostId}
                              className={({ active }) =>
                                classNames(
                                  active
                                    ? "text-white bg-indigo-600"
                                    : "text-gray-900",
                                  "cursor-default select-none relative py-2 pl-3 pr-9"
                                )
                              }
                              value={boost}
                            >
                              {({ selected, active }) => (
                                <>
                                  <div className="flex items-start">
                                    <span
                                      className={classNames(
                                        boost.boostActive
                                          ? "bg-green-400"
                                          : "bg-red-400",
                                        "flex-shrink-0 inline-block h-2 w-2 rounded-full mt-1.5"
                                      )}
                                      aria-hidden="true"
                                    />
                                    <span
                                      className={classNames(
                                        selected
                                          ? "font-semibold"
                                          : "font-normal",
                                        "ml-3 block truncate"
                                      )}
                                    >
                                      {boost.boostName}
                                      <div>
                                        {boost.boostRewardInBips / 100}%
                                      </div>
                                      <span className="sr-only">
                                        {" "}
                                        is{" "}
                                        {boost.boostActive
                                          ? "online"
                                          : "offline"}
                                      </span>
                                    </span>
                                  </div>

                                  {selected ? (
                                    <span
                                      className={classNames(
                                        active
                                          ? "text-white"
                                          : "text-indigo-600",
                                        "absolute inset-y-0 right-0 flex items-center pr-4"
                                      )}
                                    >
                                      <CheckIcon
                                        className="h-5 w-5"
                                        aria-hidden="true"
                                      />
                                    </span>
                                  ) : null}
                                </>
                              )}
                            </Listbox.Option>
                          ))}
                        </Listbox.Options>
                      </Transition>
                    </div>
                  </div>
                )}
              </Listbox>

              <button
                onClick={() => {
                  if (selected === null) {
                    throw new Error("must select a boost");
                  }
                  activateBoost.mutate(selected.boostId);
                }}
                className="justify-center py-0 px-4 border border-transparent shadow-sm text-sm font-medium rounded-md text-white focus:outline-none focus:ring-2 focus:ring-offset-2 bg-indigo-600 hover:bg-indigo-700 focus:ring-indigo-500"
              >
                Activate
              </button>
            </div>
          </div>
        </div>
      </>
    );
  } else if (boosts.status === "loading" || allBoosts.status === "loading") {
    content = (
      <>
        {content}
        <div>Boosts {boosts.status}</div>
        <div>All Boosts {allBoosts.status}</div>
      </>
    );
  } else {
    content = (
      <>
        {content}
        <div>Boosts {boosts.status}</div>
        <div>All Boosts {allBoosts.status}</div>
      </>
    );
  }

  switch (boosts.status) {
    case "success":
      content = (
        <>
          {content}
          <div className="h-4" />
          <BoostLists boosts={boosts.data} />
        </>
      );
      break;

    case "loading":
      content = (
        <>
          {content}
          <div className="h-4" />
          <div>boosts {boosts.status}</div>
        </>
      );
      break;

    case "error":
      content = (
        <>
          {content}
          <div className="h-4" />
          <div>error loading boosts</div>
        </>
      );
      break;

    case "idle":
      break;
  }

  switch (activations.status) {
    case "success":
      content = (
        <>
          {content}
          <div className="h-4" />
          <ActivationsList
            activations={activations.data}
            group={activeGroup?.id}
          />
        </>
      );
      break;

    case "loading":
      content = (
        <>
          {content}
          <div className="h-4" />
          <div>activations {activations.status}</div>
        </>
      );
      break;

    case "error":
      content = (
        <>
          {content}
          <div className="h-4" />
          <div>error loading activations</div>
        </>
      );
      break;

    case "idle":
      break;
  }

  return content;
}
