/* eslint-disable @typescript-eslint/no-unsafe-member-access */
/* eslint-disable @typescript-eslint/no-explicit-any */

import { FormEvent, Fragment, ReactNode, useRef, useState } from "react";
import { defaultTo } from "lodash";
import { Dialog, Listbox, Transition } from "@headlessui/react";
import {
  CheckIcon,
  ChevronDownIcon,
  ExclamationIcon,
} from "@heroicons/react/solid";
import { Link } from "react-router-dom";

import useGetUserModel from "./UseGetUserModel";
import {
  useChangeAddress,
  useChangeBank,
  useChangeDOB,
  useChangeEmail,
  useChangeName,
  useChangePhone,
  useChangeSSN,
  useCloseUser,
  useCreateDwolla,
  useCreatePrivacy,
} from "./UseChangeUser";
import {
  useManualFSVerify,
  useRemoveFS,
  useResendFSVerification,
} from "./UseChangeFS";
import { UserModel } from "./Types";
import Spinner from "../Shared/Spinner";

function classNames(...classes: string[]) {
  return classes.filter(Boolean).join(" ");
}

interface Props {
  user: string;
}

const Row = (props: {
  left: ReactNode;
  right: ReactNode;
  button?: { text: string; onClick: () => void };
}) => (
  <div className="py-4 sm:py-5 sm:grid sm:grid-cols-3 sm:gap-4">
    <dt className="text-sm font-medium text-gray-500">{props.left}</dt>
    <dd className="mt-1 flex text-sm text-gray-900 sm:mt-0 sm:col-span-2 items-center">
      <span className="flex-grow">{props.right}</span>
      {props.button === undefined ? (
        <></>
      ) : (
        <span className="ml-4 flex-shrink-0">
          <button
            onClick={props.button.onClick}
            type="button"
            className="bg-white rounded-md font-medium text-indigo-600 hover:text-indigo-500 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
          >
            {props.button.text}
          </button>
        </span>
      )}
    </dd>
  </div>
);

const closeReasons = [
  {
    title: "ClosedByUser",
    description: "The user asked for their account to be closed",
  },
  { title: "KYCFailed", description: "The user did not pass KYC" },
  {
    title: "FraudyUser",
    description: "The user was fruadulent in their intents",
  },
  {
    title: "OverdueBalance",
    description: "The user has an overdue balance past the maximum period",
  },
  {
    title: "DuplicateUser",
    description: "This user account is a duplicate of another user account",
  },
  {
    title: "ForeignDeviceDuringSignup",
    description: "Not using a US device to signup",
  },
  {
    title: "Archived",
    description: "Too long to finish signing up or to make 1st purchase",
  },
  { title: "BannedDeviceId", description: "Banned Device" },
  { title: "BannedIPAddress", description: "Banned IP Address" },
  { title: "BannedDeviceType", description: "Banned Device Type" },
];

export default function UserModelView({ user }: Props): JSX.Element {
  const model = useGetUserModel(user);
  const createPrivacy = useCreatePrivacy(user);
  const createDwolla = useCreateDwolla(user);
  const changeName = useChangeName(user);
  const changeEmail = useChangeEmail(user);
  const changePhone = useChangePhone(user);
  const changeAddress = useChangeAddress(user);
  const changeBank = useChangeBank(user);
  const changeSSN = useChangeSSN(user);
  const changeDOB = useChangeDOB(user);

  const resendFS = useResendFSVerification(user);
  const manualVerify = useManualFSVerify(user);
  const removeFS = useRemoveFS(user);

  const [editingName, setEditName] = useState(false);
  const [editingEmail, setEditEmail] = useState(false);
  const [editingPhone, setEditPhone] = useState(false);
  const [editingAddress, setEditAddress] = useState(false);
  const [editingBank, setEditBank] = useState(false);
  const [editingSSN, setEditSSN] = useState(false);
  const [editingDOB, setEditDOB] = useState(false);

  const [closeReason, setCloseReason] = useState<null | string>(null);
  const closeUser = useCloseUser(user);

  return model.status === "success" ? (
    <>
      <div>
        <h3 className="text-lg leading-6 font-medium text-gray-900">
          {model.data.firstname} {model.data.lastname}
        </h3>
        <p className="mt-1 max-w-2xl text-sm text-gray-500">
          {model.data.email}
        </p>
      </div>
      <div className="mt-5 border-t border-gray-200">
        <dl className="divide-y divide-gray-200">
          <Row
            left={
              <>
                <div className="flex justify-between items-center">
                  <span>Full Name</span>
                  <button
                    onClick={() => setEditName(!editingName)}
                    type="button"
                    className="bg-white rounded-md font-medium text-indigo-600 hover:text-indigo-500 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500 border-2 border-indigo-500 p-1 px-1"
                  >
                    Edit
                  </button>
                </div>
              </>
            }
            right={
              editingName ? (
                <form
                  className="flex items-end"
                  onSubmit={async (event: FormEvent<HTMLFormElement>) => {
                    event.preventDefault();
                    const elements = event.currentTarget.elements as any;
                    const fname = (elements.fname as HTMLInputElement).value;
                    const lname = (elements.lname as HTMLInputElement).value;
                    await changeName.mutateAsync({ first: fname, last: lname });
                    setEditName(false);
                  }}
                >
                  <div className="isolate -space-y-px rounded-md shadow-sm w-80">
                    <div className="relative border border-gray-300 rounded-md rounded-b-none px-3 py-2 focus-within:z-10 focus-within:ring-1 focus-within:ring-indigo-600 focus-within:border-indigo-600">
                      <label
                        htmlFor="fname"
                        className="block text-xs font-medium text-gray-700"
                      >
                        First Name
                      </label>
                      <input
                        type="text"
                        name="fname"
                        id="fname"
                        className="block border-0 p-0 text-gray-900 placeholder-gray-300 focus:ring-0 sm:text-sm"
                        placeholder="Jane"
                        defaultValue={defaultTo(
                          model.data.firstname,
                          undefined
                        )}
                      />
                    </div>
                    <div className="relative border border-gray-300 rounded-md rounded-t-none px-3 py-2 focus-within:z-10 focus-within:ring-1 focus-within:ring-indigo-600 focus-within:border-indigo-600">
                      <label
                        htmlFor="lname"
                        className="block w-full text-xs font-medium text-gray-700"
                      >
                        Last Name
                      </label>
                      <input
                        type="text"
                        name="lname"
                        id="lname"
                        className="block w-full border-0 p-0 text-gray-900 placeholder-gray-300 focus:ring-0 sm:text-sm"
                        placeholder="Doe"
                        defaultValue={defaultTo(model.data.lastname, undefined)}
                      />
                    </div>
                  </div>
                  <button
                    type="submit"
                    disabled={changeName.isLoading}
                    className="ml-2 inline-flex items-center px-4 py-2 border border-transparent text-sm font-medium rounded-md shadow-sm text-white bg-indigo-600 hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
                  >
                    {changeName.isLoading ? "..." : "Update"}
                  </button>
                </form>
              ) : (
                <div className="flex justify-between">
                  <div>
                    {defaultTo(model.data.firstname, "<no fname>")}{" "}
                    {defaultTo(model.data.lastname, "<no lname>")}
                  </div>
                  <div>
                    <Link
                      to={`/users/search/text?q=${defaultTo(
                        model.data.firstname,
                        ""
                      )}%20${defaultTo(model.data.lastname, "")}`}
                      className="ml-2 inline-flex items-center px-4 py-2 border border-transparent text-sm font-medium rounded-md shadow-sm text-white bg-indigo-600 hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
                    >
                      Search full name
                    </Link>
                    <Link
                      to={`/users/search/text?q=${defaultTo(
                        model.data.lastname,
                        ""
                      )}`}
                      className="ml-2 inline-flex items-center px-4 py-2 border border-transparent text-sm font-medium rounded-md shadow-sm text-white bg-indigo-600 hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
                    >
                      Search last name
                    </Link>
                  </div>
                </div>
              )
            }
          />
          <Row left="User Id" right={model.data.userid} />
          <Row
            left="Status"
            right={
              <div className="flex justify-between items-center">
                <div>
                  {model.data.userstate.tag}
                  {model.data.userstate.tag === "UserClosed" ? (
                    <div>{model.data.userstate.contents}</div>
                  ) : (
                    <></>
                  )}
                </div>
                <Listbox value={closeReason} onChange={setCloseReason}>
                  {({ open }) => (
                    <>
                      <Listbox.Label className="sr-only">
                        Change published status
                      </Listbox.Label>
                      <div className="relative">
                        <div className="inline-flex shadow-sm rounded-md divide-x divide-red-600">
                          <div className="relative z-0 inline-flex shadow-sm rounded-md divide-x divide-red-600">
                            <div className="relative inline-flex items-center bg-red-500 py-2 pl-3 pr-4 border border-transparent rounded-l-md shadow-sm text-white">
                              <p className="ml-2.5 text-sm font-medium">
                                Close user for reason
                              </p>
                            </div>
                            <Listbox.Button className="relative inline-flex items-center bg-red-500 p-2 rounded-l-none rounded-r-md text-sm font-medium text-white hover:bg-red-600 focus:outline-none focus:z-10 focus:ring-2 focus:ring-offset-2 focus:ring-offset-gray-50 focus:ring-red-500">
                              <span className="sr-only">
                                Change published status
                              </span>
                              <ChevronDownIcon
                                className="h-5 w-5 text-white"
                                aria-hidden="true"
                              />
                            </Listbox.Button>
                          </div>
                        </div>

                        <Transition
                          show={open}
                          as={Fragment}
                          leave="transition ease-in duration-100"
                          leaveFrom="opacity-100"
                          leaveTo="opacity-0"
                        >
                          <Listbox.Options className="origin-top-right absolute z-10 right-0 mt-2 w-72 rounded-md shadow-lg overflow-hidden bg-white divide-y divide-gray-200 ring-1 ring-black ring-opacity-5 focus:outline-none">
                            {closeReasons.map((option) => (
                              <Listbox.Option
                                key={option.title}
                                className={({ active }) =>
                                  classNames(
                                    active
                                      ? "text-white bg-red-500"
                                      : "text-gray-900",
                                    "cursor-default select-none relative p-4 text-sm"
                                  )
                                }
                                value={option.title}
                              >
                                {({ selected, active }) => (
                                  <div className="flex flex-col">
                                    <div className="flex justify-between">
                                      <p
                                        className={
                                          selected
                                            ? "font-semibold"
                                            : "font-normal"
                                        }
                                      >
                                        {option.title}
                                      </p>
                                      {selected ? (
                                        <span
                                          className={
                                            active
                                              ? "text-white"
                                              : "text-red-500"
                                          }
                                        >
                                          <CheckIcon
                                            className="h-5 w-5"
                                            aria-hidden="true"
                                          />
                                        </span>
                                      ) : null}
                                    </div>
                                    <p
                                      className={classNames(
                                        active
                                          ? "text-red-200"
                                          : "text-gray-500",
                                        "mt-2"
                                      )}
                                    >
                                      {option.description}
                                    </p>
                                  </div>
                                )}
                              </Listbox.Option>
                            ))}
                          </Listbox.Options>
                        </Transition>
                      </div>
                    </>
                  )}
                </Listbox>
              </div>
            }
          />
          {/* New row */}
          <Row
            left={
              <>
                <div className="flex justify-between items-center">
                  <span>Email</span>
                  <button
                    onClick={() => setEditEmail(!editingEmail)}
                    type="button"
                    className="bg-white rounded-md font-medium text-indigo-600 hover:text-indigo-500 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500 border-2 border-indigo-500 p-1 px-1"
                  >
                    Edit
                  </button>
                </div>
              </>
            }
            right={
              editingEmail ? (
                <>
                  <form
                    className="flex items-end"
                    onSubmit={async (event: FormEvent<HTMLFormElement>) => {
                      event.preventDefault();
                      const elements = event.currentTarget.elements as any;
                      const email = (elements.email as HTMLInputElement).value;
                      await changeEmail.mutateAsync({
                        email: email,
                      });
                      setEditEmail(false);
                    }}
                  >
                    <div className="isolate -space-y-px rounded-md shadow-sm w-80">
                      <div className="relative border border-gray-300 rounded-md px-3 py-2 focus-within:z-10 focus-within:ring-1 focus-within:ring-indigo-600 focus-within:border-indigo-600">
                        <label
                          htmlFor="email"
                          className="block text-xs font-medium text-gray-700"
                        >
                          Email
                        </label>
                        <input
                          type="text"
                          name="email"
                          id="email"
                          className="block border-0 p-0 text-gray-900 placeholder-gray-300 focus:ring-0 sm:text-sm"
                          placeholder="Jane.doe@gmail.com"
                          defaultValue={model.data.email}
                        />
                      </div>
                    </div>
                    <button
                      disabled={changeEmail.isLoading}
                      type="submit"
                      className="ml-2 inline-flex items-center px-4 py-2 border border-transparent text-sm font-medium rounded-md shadow-sm text-white bg-indigo-600 hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
                    >
                      Update
                    </button>
                  </form>
                </>
              ) : (
                <>
                  <span>{model.data.email}</span>{" "}
                  <span
                    className={`inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-${
                      model.data.emailverified ? "green" : "red"
                    }-100 text-${
                      model.data.emailverified ? "green" : "red"
                    }-800`}
                  >
                    {model.data.emailverified ? "Verified" : "Not Verified"}
                  </span>
                </>
              )
            }
          />

          {/* New row */}
          <Row
            left={
              <>
                <div className="flex justify-between items-center">
                  <span>Phone</span>
                  <button
                    onClick={() => setEditPhone(!editingPhone)}
                    type="button"
                    className="bg-white rounded-md font-medium text-indigo-600 hover:text-indigo-500 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500 border-2 border-indigo-500 p-1 px-1"
                  >
                    Edit
                  </button>
                </div>
              </>
            }
            right={
              editingPhone ? (
                <>
                  <form
                    className="flex items-end"
                    onSubmit={async (event: FormEvent<HTMLFormElement>) => {
                      event.preventDefault();
                      const elements = event.currentTarget.elements as any;
                      const phone = (elements.phone as HTMLInputElement).value;
                      await changePhone.mutateAsync({
                        phone: phone,
                      });
                      setEditPhone(false);
                    }}
                  >
                    <div className="isolate -space-y-px rounded-md shadow-sm w-80">
                      <div className="relative border border-gray-300 rounded-md px-3 py-2 focus-within:z-10 focus-within:ring-1 focus-within:ring-indigo-600 focus-within:border-indigo-600">
                        <label
                          htmlFor="phone"
                          className="block text-xs font-medium text-gray-700"
                        >
                          Phone
                        </label>
                        <input
                          type="text"
                          name="phone"
                          id="phone"
                          className="block border-0 p-0 text-gray-900 placeholder-gray-300 focus:ring-0 sm:text-sm"
                          placeholder="9491239876"
                          defaultValue={defaultTo(model.data.phone, undefined)}
                        />
                      </div>
                    </div>
                    <button
                      disabled={changePhone.isLoading}
                      type="submit"
                      className="ml-2 inline-flex items-center px-4 py-2 border border-transparent text-sm font-medium rounded-md shadow-sm text-white bg-indigo-600 hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
                    >
                      Update
                    </button>
                  </form>
                </>
              ) : (
                <>
                  <span>{defaultTo(model.data.phone, "<None>")}</span>{" "}
                  <span
                    className={`inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-${
                      model.data.phoneverified ? "green" : "red"
                    }-100 text-${
                      model.data.phoneverified ? "green" : "red"
                    }-800`}
                  >
                    {model.data.phoneverified ? "Verified" : "Not Verified"}
                  </span>
                </>
              )
            }
          />
          <Row
            left={
              model.data.privacyaccttoken === null ? (
                <>
                  <div className="flex justify-between items-center">
                    <span>Privacy Account Token</span>
                    <button
                      disabled={createPrivacy.isLoading}
                      onClick={() => createPrivacy.mutate()}
                      type="button"
                      className="bg-white rounded-md font-medium text-indigo-600 hover:text-indigo-500 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500 border-2 border-indigo-500 p-1 px-1"
                    >
                      {createPrivacy.isLoading ? "..." : "Create"}
                    </button>
                  </div>
                </>
              ) : (
                "Privacy Account Token"
              )
            }
            right={defaultTo(model.data.privacyaccttoken, "<None>")}
          />
          <Row
            left={
              <>
                <div className="flex justify-between items-center">
                  <span>Address</span>
                  <button
                    onClick={() => setEditAddress(!editingAddress)}
                    type="button"
                    className="bg-white rounded-md font-medium text-indigo-600 hover:text-indigo-500 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500 border-2 border-indigo-500 p-1 px-1"
                  >
                    Edit
                  </button>
                </div>
              </>
            }
            right={
              editingAddress ? (
                <>
                  <form
                    className="flex items-end"
                    onSubmit={async (event: FormEvent<HTMLFormElement>) => {
                      event.preventDefault();
                      const elements = event.currentTarget.elements as any;
                      const street = (elements.street as HTMLInputElement)
                        .value;
                      const city = (elements.city as HTMLInputElement).value;
                      const state = (elements.state as HTMLInputElement).value;
                      const zip = (elements.zip as HTMLInputElement).value;
                      await changeAddress.mutateAsync({
                        street,
                        city,
                        state,
                        zip,
                      });
                      setEditAddress(false);
                    }}
                  >
                    <div className="isolate -space-y-px rounded-md shadow-sm w-80">
                      <div className="relative border border-gray-300 rounded-md rounded-b-none px-3 py-2 focus-within:z-10 focus-within:ring-1 focus-within:ring-indigo-600 focus-within:border-indigo-600">
                        <label
                          htmlFor="street"
                          className="block text-xs font-medium text-gray-700"
                        >
                          Street
                        </label>
                        <input
                          type="text"
                          name="street"
                          id="street"
                          className="block border-0 p-0 text-gray-900 placeholder-gray-300 focus:ring-0 sm:text-sm"
                          placeholder="123 main st"
                          defaultValue={defaultTo(
                            model.data.addressstreet,
                            undefined
                          )}
                        />
                      </div>
                      <div className="relative border border-gray-300  px-3 py-2 focus-within:z-10 focus-within:ring-1 focus-within:ring-indigo-600 focus-within:border-indigo-600">
                        <label
                          htmlFor="city"
                          className="block w-full text-xs font-medium text-gray-700"
                        >
                          City
                        </label>
                        <input
                          type="text"
                          name="city"
                          id="city"
                          className="block w-full border-0 p-0 text-gray-900 placeholder-gray-300 focus:ring-0 sm:text-sm"
                          placeholder="Irvine"
                          defaultValue={defaultTo(
                            model.data.addresscity,
                            undefined
                          )}
                        />
                      </div>
                      <div className="relative border border-gray-300  px-3 py-2 focus-within:z-10 focus-within:ring-1 focus-within:ring-indigo-600 focus-within:border-indigo-600">
                        <label
                          htmlFor="state"
                          className="block w-full text-xs font-medium text-gray-700"
                        >
                          State
                        </label>
                        <input
                          type="text"
                          name="state"
                          id="state"
                          className="block w-full border-0 p-0 text-gray-900 placeholder-gray-300 focus:ring-0 sm:text-sm"
                          placeholder="CA"
                          defaultValue={defaultTo(
                            model.data.addressstate,
                            undefined
                          )}
                        />
                      </div>
                      <div className="relative border border-gray-300 rounded-md rounded-t-none px-3 py-2 focus-within:z-10 focus-within:ring-1 focus-within:ring-indigo-600 focus-within:border-indigo-600">
                        <label
                          htmlFor="zip"
                          className="block w-full text-xs font-medium text-gray-700"
                        >
                          Zip
                        </label>
                        <input
                          type="text"
                          name="zip"
                          id="zip"
                          className="block w-full border-0 p-0 text-gray-900 placeholder-gray-300 focus:ring-0 sm:text-sm"
                          placeholder="98765"
                          defaultValue={defaultTo(
                            model.data.addresszip,
                            undefined
                          )}
                        />
                      </div>
                    </div>
                    <button
                      disabled={changeAddress.isLoading}
                      type="submit"
                      className="ml-2 inline-flex items-center px-4 py-2 border border-transparent text-sm font-medium rounded-md shadow-sm text-white bg-indigo-600 hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
                    >
                      Update
                    </button>
                  </form>
                </>
              ) : (
                <div className="flex justify-between">
                  <div>
                    <ShowAddress model={model.data} />
                  </div>
                  <div>
                    {model.data.addressstreet !== null ? (
                      <Link
                        to={`/users/search/address?q=${model.data.addressstreet}`}
                        className="ml-2 inline-flex items-center px-4 py-2 border border-transparent text-sm font-medium rounded-md shadow-sm text-white bg-indigo-600 hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
                      >
                        Search street
                      </Link>
                    ) : (
                      <></>
                    )}
                    {model.data.addresszip !== null ? (
                      <Link
                        to={`/users/search/address?q=${model.data.addresszip}`}
                        className="ml-2 inline-flex items-center px-4 py-2 border border-transparent text-sm font-medium rounded-md shadow-sm text-white bg-indigo-600 hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
                      >
                        Search zip
                      </Link>
                    ) : (
                      <></>
                    )}
                  </div>
                </div>
              )
            }
          />
          <Row
            left="Bank Verified"
            right={
              <div className="flex justify-between items-center">
                <span
                  className={`inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-${
                    model.data.bankverified ? "green" : "red"
                  }-100 text-${model.data.bankverified ? "green" : "red"}-800`}
                >
                  {model.data.bankverified ? "Verified" : "Not Verified"}
                </span>
                <div className="flex items-center">
                  <button
                    disabled={resendFS.isLoading}
                    onClick={() => resendFS.mutate()}
                    className="ml-2 inline-flex items-center px-4 py-2 border border-transparent text-sm font-medium rounded-md shadow-sm text-white bg-indigo-600 hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
                  >
                    {resendFS.isLoading ? <Spinner /> : "Resend Verifcation"}
                  </button>
                  <button
                    disabled={manualVerify.isLoading}
                    onClick={() => manualVerify.mutate()}
                    className="ml-2 inline-flex items-center px-4 py-2 border border-transparent text-sm font-medium rounded-md shadow-sm text-white bg-indigo-600 hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
                  >
                    {manualVerify.isLoading ? (
                      <Spinner />
                    ) : (
                      "Manually verify FS"
                    )}
                  </button>
                  <button
                    disabled={removeFS.isLoading}
                    onClick={() => removeFS.mutate()}
                    className="ml-2 inline-flex items-center px-4 py-2 border border-transparent text-sm font-medium rounded-md shadow-sm text-white bg-indigo-600 hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
                  >
                    {removeFS.isLoading ? <Spinner /> : "Remove FS"}
                  </button>
                </div>
              </div>
            }
          />
          <Row
            left={
              <>
                <div className="flex justify-between items-center">
                  <span>Bank details</span>
                  <button
                    onClick={() => setEditBank(!editingBank)}
                    type="button"
                    className="bg-white rounded-md font-medium text-indigo-600 hover:text-indigo-500 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500 border-2 border-indigo-500 p-1 px-1"
                  >
                    Edit
                  </button>
                </div>
              </>
            }
            right={
              editingBank ? (
                <>
                  <form
                    className="flex items-end"
                    onSubmit={async (event: FormEvent<HTMLFormElement>) => {
                      event.preventDefault();
                      const elements = event.currentTarget.elements as any;
                      const bankname = (elements.bankname as HTMLInputElement)
                        .value;
                      const accountname = (
                        elements.accountname as HTMLInputElement
                      ).value;
                      const routingnumber = (
                        elements.routing as HTMLInputElement
                      ).value;
                      const accountnumber = (
                        elements.accountnum as HTMLInputElement
                      ).value;
                      await changeBank.mutateAsync({
                        bankname,
                        accountname,
                        routingnumber,
                        accountnumber,
                      });
                      setEditBank(false);
                    }}
                  >
                    <div className="isolate -space-y-px rounded-md shadow-sm w-80">
                      <div className="relative border border-gray-300 rounded-md rounded-b-none px-3 py-2 focus-within:z-10 focus-within:ring-1 focus-within:ring-indigo-600 focus-within:border-indigo-600">
                        <label
                          htmlFor="bankname"
                          className="block text-xs font-medium text-gray-700"
                        >
                          Bank name
                        </label>
                        <input
                          type="text"
                          name="bankname"
                          id="bankname"
                          className="block border-0 p-0 text-gray-900 placeholder-gray-300 focus:ring-0 sm:text-sm"
                          placeholder="Bank of Tgthr"
                          defaultValue={defaultTo(
                            model.data.bankname,
                            undefined
                          )}
                        />
                      </div>
                      <div className="relative border border-gray-300  px-3 py-2 focus-within:z-10 focus-within:ring-1 focus-within:ring-indigo-600 focus-within:border-indigo-600">
                        <label
                          htmlFor="accountname"
                          className="block w-full text-xs font-medium text-gray-700"
                        >
                          Account name
                        </label>
                        <input
                          type="text"
                          name="accountname"
                          id="accountname"
                          className="block w-full border-0 p-0 text-gray-900 placeholder-gray-300 focus:ring-0 sm:text-sm"
                          placeholder="Freedom Fries Checking"
                          defaultValue={defaultTo(
                            model.data.bankaccountname,
                            undefined
                          )}
                        />
                      </div>
                      <div className="relative border border-gray-300  px-3 py-2 focus-within:z-10 focus-within:ring-1 focus-within:ring-indigo-600 focus-within:border-indigo-600">
                        <label
                          htmlFor="routing"
                          className="block w-full text-xs font-medium text-gray-700"
                        >
                          Routing number
                        </label>
                        <input
                          type="text"
                          name="routing"
                          id="routing"
                          className="block w-full border-0 p-0 text-gray-900 placeholder-gray-300 focus:ring-0 sm:text-sm"
                          placeholder="987333232"
                          defaultValue={defaultTo(
                            model.data.bankrouting,
                            undefined
                          )}
                        />
                      </div>
                      <div className="relative border border-gray-300 rounded-md rounded-t-none px-3 py-2 focus-within:z-10 focus-within:ring-1 focus-within:ring-indigo-600 focus-within:border-indigo-600">
                        <label
                          htmlFor="accountnum"
                          className="block w-full text-xs font-medium text-gray-700"
                        >
                          Account Number
                        </label>
                        <input
                          type="text"
                          name="accountnum"
                          id="accountnum"
                          className="block w-full border-0 p-0 text-gray-900 placeholder-gray-300 focus:ring-0 sm:text-sm"
                          placeholder="5252525252525252"
                          defaultValue={defaultTo(
                            model.data.bankacount,
                            undefined
                          )}
                        />
                      </div>
                    </div>
                    <button
                      disabled={changeBank.isLoading}
                      type="submit"
                      className="ml-2 inline-flex items-center px-4 py-2 border border-transparent text-sm font-medium rounded-md shadow-sm text-white bg-indigo-600 hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
                    >
                      Update
                    </button>
                  </form>
                </>
              ) : (
                <>
                  <div>
                    Bank name: {defaultTo(model.data.bankname, "<None>")}
                  </div>
                  <div>
                    Account name:{" "}
                    {defaultTo(model.data.bankaccountname, "<None>")}
                  </div>
                  <div>
                    Bank routing: {defaultTo(model.data.bankrouting, "<None>")}
                  </div>
                  <div>
                    Bank acct #: {defaultTo(model.data.bankacount, "<None>")}
                  </div>
                </>
              )
            }
          />
          <Row
            left={
              model.data.dwollaid === null ? (
                <>
                  <div className="flex justify-between items-center">
                    <span>Dwolla ID</span>
                    <button
                      disabled={createDwolla.isLoading}
                      onClick={() => createDwolla.mutate()}
                      type="button"
                      className="bg-white rounded-md font-medium text-indigo-600 hover:text-indigo-500 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500 border-2 border-indigo-500 p-1 px-1"
                    >
                      {createDwolla.isLoading ? "..." : "Create"}
                    </button>
                  </div>
                </>
              ) : (
                "Dwolla ID"
              )
            }
            right={defaultTo(model.data.dwollaid, "<None>")}
          />
          <Row
            left="Dwolla FS ID"
            right={defaultTo(model.data.dwollafundingid, "<None>")}
          />
          <Row
            left={
              <div className="flex justify-between items-center">
                <span>DOB</span>
                <div className="flex">
                  <button
                    onClick={() => setEditDOB(!editingDOB)}
                    type="button"
                    className="bg-white rounded-md font-medium text-indigo-600 hover:text-indigo-500 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500 border-2 border-indigo-500 p-1 px-1"
                  >
                    Edit
                  </button>
                </div>
              </div>
            }
            right={
              editingDOB ? (
                <form
                  className="flex items-end"
                  onSubmit={async (event: FormEvent<HTMLFormElement>) => {
                    event.preventDefault();
                    const elements = event.currentTarget.elements as any;
                    const dob = (elements.dob as HTMLInputElement).value;
                    await changeDOB.mutateAsync({
                      dob,
                    });
                    setEditDOB(false);
                  }}
                >
                  <div className="isolate -space-y-px rounded-md shadow-sm w-80">
                    <div className="relative border border-gray-300 rounded-md px-3 py-2 focus-within:z-10 focus-within:ring-1 focus-within:ring-indigo-600 focus-within:border-indigo-600">
                      <label
                        htmlFor="dob"
                        className="block text-xs font-medium text-gray-700"
                      >
                        DOB (MM/DD/YYYY)
                      </label>
                      <input
                        type="text"
                        name="dob"
                        id="dob"
                        className="block border-0 p-0 text-gray-900 placeholder-gray-300 focus:ring-0 sm:text-sm"
                        placeholder="01/23/1989"
                        defaultValue={
                          model.data.dob === null
                            ? ""
                            : new Intl.DateTimeFormat("en-US", {
                                year: "numeric",
                                month: "2-digit",
                                day: "2-digit",
                                timeZone: "UTC",
                              }).format(new Date(model.data.dob))
                        }
                      />
                    </div>
                  </div>
                  <button
                    disabled={changeSSN.isLoading}
                    type="submit"
                    className="ml-2 inline-flex items-center px-4 py-2 border border-transparent text-sm font-medium rounded-md shadow-sm text-white bg-indigo-600 hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
                  >
                    Update
                  </button>
                </form>
              ) : model.data.dob === null ? (
                "<None>"
              ) : (
                <>
                  <div>
                    {new Date(model.data.dob).toLocaleDateString("en-US", {
                      timeZone: "UTC",
                    })}
                  </div>
                  <div>{calculateAge(new Date(model.data.dob))} years old</div>
                </>
              )
            }
          />
          <Row
            left={
              <>
                <div className="flex justify-between items-center">
                  <span>SSN</span>
                  <div className="flex">
                    {model.data.ssn === null ? (
                      <></>
                    ) : (
                      <button
                        type="button"
                        className="mr-2 bg-white rounded-md font-medium text-indigo-600 hover:text-indigo-500 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500 border-2 border-indigo-500 p-1 px-1"
                      >
                        View
                      </button>
                    )}
                    <button
                      onClick={() => setEditSSN(!editingSSN)}
                      type="button"
                      className="bg-white rounded-md font-medium text-indigo-600 hover:text-indigo-500 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500 border-2 border-indigo-500 p-1 px-1"
                    >
                      Edit
                    </button>
                  </div>
                </div>
              </>
            }
            right={
              editingSSN ? (
                <>
                  <form
                    className="flex items-end"
                    onSubmit={async (event: FormEvent<HTMLFormElement>) => {
                      event.preventDefault();
                      const elements = event.currentTarget.elements as any;
                      const ssn = (elements.ssn as HTMLInputElement).value;
                      await changeSSN.mutateAsync({
                        ssn,
                      });
                      setEditSSN(false);
                    }}
                  >
                    <div className="isolate -space-y-px rounded-md shadow-sm w-80">
                      <div className="relative border border-gray-300 rounded-md px-3 py-2 focus-within:z-10 focus-within:ring-1 focus-within:ring-indigo-600 focus-within:border-indigo-600">
                        <label
                          htmlFor="ssn"
                          className="block text-xs font-medium text-gray-700"
                        >
                          SSN
                        </label>
                        <input
                          type="text"
                          name="ssn"
                          id="ssn"
                          className="block border-0 p-0 text-gray-900 placeholder-gray-300 focus:ring-0 sm:text-sm"
                          placeholder="123-45-6789"
                        />
                      </div>
                    </div>
                    <button
                      disabled={changeSSN.isLoading}
                      type="submit"
                      className="ml-2 inline-flex items-center px-4 py-2 border border-transparent text-sm font-medium rounded-md shadow-sm text-white bg-indigo-600 hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
                    >
                      Update
                    </button>
                  </form>
                </>
              ) : model.data.ssn === null ? (
                "<None>"
              ) : (
                <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-green-100 text-green-800">
                  Entered
                </span>
              )
            }
          />
          <Row
            left="Created On"
            right={new Date(model.data.createdon).toLocaleString()}
          />
          <Row
            left="First signin on"
            right={
              model.data.firstsignin === null ? (
                <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-red-100 text-red-800">
                  None
                </span>
              ) : (
                new Date(model.data.firstsignin).toLocaleString()
              )
            }
          />
          <Row
            left="Disclose accepted Last"
            right={
              model.data.dislcosureok === null ? (
                <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-red-100 text-red-800">
                  None
                </span>
              ) : (
                new Date(model.data.dislcosureok).toLocaleString()
              )
            }
          />
          <Row
            left="Consent accepted Last"
            right={
              model.data.constentok === null ? (
                <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-red-100 text-red-800">
                  None
                </span>
              ) : (
                new Date(model.data.constentok).toLocaleString()
              )
            }
          />
          <Row
            left="Became last active on"
            right={
              model.data.becameactiveon === null ? (
                <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-red-100 text-red-800">
                  None
                </span>
              ) : (
                new Date(model.data.becameactiveon).toLocaleString()
              )
            }
          />
        </dl>
      </div>
      {closeReason === null ? (
        <></>
      ) : (
        <CloseUserModal
          reason={closeReason}
          cancel={() => setCloseReason(null)}
          action={async () => {
            await closeUser.mutateAsync(closeReason);
            void setCloseReason(null);
          }}
        />
      )}
    </>
  ) : (
    <div>loading user info</div>
  );
}

const ShowAddress = ({ model }: { model: UserModel }) => (
  <>
    <div>
      {model.addressstreet === null ? (
        <span className="text-gray-300">No street address</span>
      ) : (
        <span>{model.addressstreet}</span>
      )}
    </div>
    <div>
      {model.addressstreet2 === null ? (
        <span className="text-gray-300">No 2nd line street</span>
      ) : (
        <span>{model.addressstreet2}</span>
      )}
    </div>
    <div>
      {model.addresscity === null ? (
        <span className="text-gray-300">No city</span>
      ) : (
        <span>{model.addresscity}</span>
      )}
      ,{" "}
      {model.addressstate === null ? (
        <span className="text-gray-300">No state</span>
      ) : (
        <span>{model.addressstate}</span>
      )}{" "}
      {model.addresszip === null ? (
        <span className="text-gray-300">No ZIP</span>
      ) : (
        <span>{model.addresszip}</span>
      )}
    </div>
  </>
);

function calculateAge(birthday: Date) {
  // birthday is a date
  const ageDifMs = Date.now() - birthday.valueOf();
  const ageDate = new Date(ageDifMs); // miliseconds from epoch
  return Math.abs(ageDate.getUTCFullYear() - 1970);
}

function CloseUserModal(props: {
  reason: string;
  cancel: () => void;
  action: () => void;
}): JSX.Element {
  const [open, setOpen] = useState(true);

  const cancelButtonRef = useRef(null);

  return (
    <Transition.Root show={open} as={Fragment}>
      <Dialog
        as="div"
        className="fixed z-10 inset-0 overflow-y-auto"
        initialFocus={cancelButtonRef}
        onClose={setOpen}
      >
        <div className="flex items-end justify-center min-h-screen pt-4 px-4 pb-20 text-center sm:block sm:p-0">
          <Transition.Child
            as={Fragment}
            enter="ease-out duration-300"
            enterFrom="opacity-0"
            enterTo="opacity-100"
            leave="ease-in duration-200"
            leaveFrom="opacity-100"
            leaveTo="opacity-0"
          >
            <Dialog.Overlay className="fixed inset-0 bg-gray-500 bg-opacity-75 transition-opacity" />
          </Transition.Child>

          {/* This element is to trick the browser into centering the modal contents. */}
          <span
            className="hidden sm:inline-block sm:align-middle sm:h-screen"
            aria-hidden="true"
          >
            &#8203;
          </span>
          <Transition.Child
            as={Fragment}
            enter="ease-out duration-300"
            enterFrom="opacity-0 translate-y-4 sm:translate-y-0 sm:scale-95"
            enterTo="opacity-100 translate-y-0 sm:scale-100"
            leave="ease-in duration-200"
            leaveFrom="opacity-100 translate-y-0 sm:scale-100"
            leaveTo="opacity-0 translate-y-4 sm:translate-y-0 sm:scale-95"
          >
            <div className="inline-block align-bottom bg-white rounded-lg px-4 pt-5 pb-4 text-left overflow-hidden shadow-xl transform transition-all sm:my-8 sm:align-middle sm:max-w-lg sm:w-full sm:p-6">
              <div className="sm:flex sm:items-start">
                <div className="mx-auto flex-shrink-0 flex items-center justify-center h-12 w-12 rounded-full bg-red-100 sm:mx-0 sm:h-10 sm:w-10">
                  <ExclamationIcon
                    className="h-6 w-6 text-red-600"
                    aria-hidden="true"
                  />
                </div>
                <div className="mt-3 text-center sm:mt-0 sm:ml-4 sm:text-left">
                  <Dialog.Title
                    as="h3"
                    className="text-lg leading-6 font-medium text-gray-900"
                  >
                    Close user for {props.reason}?
                  </Dialog.Title>
                  <div className="mt-2">
                    <p className="text-sm text-gray-500">
                      Are you sure you want to deactivate this account? This
                      cannot be undone easily
                    </p>
                  </div>
                </div>
              </div>
              <div className="mt-5 sm:mt-4 sm:flex sm:flex-row-reverse">
                <button
                  type="button"
                  className="w-full inline-flex justify-center rounded-md border border-transparent shadow-sm px-4 py-2 bg-red-600 text-base font-medium text-white hover:bg-red-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-red-500 sm:ml-3 sm:w-auto sm:text-sm"
                  onClick={() => {
                    setOpen(false);
                    props.action();
                  }}
                >
                  Close
                </button>
                <button
                  type="button"
                  className="mt-3 w-full inline-flex justify-center rounded-md border border-gray-300 shadow-sm px-4 py-2 bg-white text-base font-medium text-gray-700 hover:bg-gray-50 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500 sm:mt-0 sm:w-auto sm:text-sm"
                  onClick={() => {
                    setOpen(false);
                    props.cancel();
                  }}
                  ref={cancelButtonRef}
                >
                  Cancel
                </button>
              </div>
            </div>
          </Transition.Child>
        </div>
      </Dialog>
    </Transition.Root>
  );
}
