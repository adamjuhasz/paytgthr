import useGetKYC from "./UseGetKYC";

interface Props {
  user: string;
}

export default function UserKYC(props: Props): JSX.Element {
  const assesments = useGetKYC(props.user);

  if (assesments.status === "success") {
    return (
      <div className="mt-1">
        <div className="bg-white shadow overflow-hidden sm:rounded-md mb-4">
          <ul>
            <li>
              <div className="px-4 py-4 sm:px-6">
                <div className="flex items-center justify-between">
                  <div className="ml-1 font-normal text-gray-500">
                    KYC Actions{" "}
                  </div>
                  <div className="ml-2 flex-shrink-0 flex">
                    <form
                      action={`/user/${props.user}/kyc/passed`}
                      method="post"
                    >
                      <button
                        type="submit"
                        className=" inline-flex items-center px-4 py-2 border border-transparent text-sm leading-5 font-medium rounded-md focus:outline-none transition ease-in-out duration-150  text-white bg-indigo-600 hover:bg-indigo-500 focus:border-indigo-700 focus:shadow-outline-indigo active:bg-indigo-700 "
                      >
                        Approve KYC
                      </button>
                    </form>
                  </div>
                  <div className="ml-2 flex-shrink-0 flex">
                    <form
                      action={`/user/${props.user}/kyc/rejected`}
                      method="post"
                    >
                      <button
                        type="submit"
                        className=" inline-flex items-center px-4 py-2 border border-transparent text-sm leading-5 font-medium rounded-md focus:outline-none transition ease-in-out duration-150  text-white bg-red-600 hover:bg-red-500 focus:border-red-700 focus:shadow-outline-red active:bg-red-700 "
                      >
                        Reject KYC
                      </button>
                    </form>
                  </div>
                  <div className="ml-2 flex-shrink-0 flex">
                    <form
                      action={`/user/${props.user}/kyc/autoverifyfailed`}
                      method="post"
                    >
                      <button
                        type="submit"
                        className=" inline-flex items-center px-4 py-2 border border-transparent text-sm leading-5 font-medium rounded-md focus:outline-none transition ease-in-out duration-150  text-white bg-indigo-600 hover:bg-indigo-500 focus:border-indigo-700 focus:shadow-outline-indigo active:bg-indigo-700 "
                      >
                        Move to Verify Failed KYC
                      </button>
                    </form>
                  </div>
                </div>
              </div>
            </li>
            <li>
              <div className="px-4 py-4 sm:px-6">
                <div className="flex items-center justify-between">
                  <div className="ml-1 font-normal text-gray-500">
                    KYC Actions{" "}
                  </div>
                  <div className="ml-2 flex-shrink-0 flex">
                    <a
                      href={`/user/${props.user}/kyc/check`}
                      className=" inline-flex items-center px-4 py-2 border border-transparent text-sm leading-5 font-medium rounded-md focus:outline-none transition ease-in-out duration-150  text-white bg-indigo-600 hover:bg-indigo-500 focus:border-indigo-700 focus:shadow-outline-indigo active:bg-indigo-700  mt-1"
                    >
                      Run new kyc
                    </a>
                  </div>
                </div>
              </div>
            </li>
          </ul>
        </div>
        <div className="mt-8 max-w-3xl mx-auto grid grid-cols-1 gap-6 sm:px-6 lg:max-w-7xl lg:grid-flow-col-dense lg:grid-cols-3">
          <div className="space-y-6 lg:col-start-1 lg:col-span-2">
            {assesments.data.map((assesment) => (
              <section
                key={assesment.createdAt}
                aria-labelledby="applicant-information-title"
              >
                <div className="bg-white shadow sm:rounded-lg">
                  <div className="px-4 py-5 sm:px-6">
                    <h2
                      id="applicant-information-title"
                      className="text-lg leading-6 font-medium text-gray-900"
                    >
                      {new Date(assesment.createdAt).toLocaleString()}
                    </h2>
                    <p className="mt-1 max-w-2xl text-sm text-gray-500 mb-2">
                      Total Score: {assesment.scoreOverall}{" "}
                      {assesment.kycPassed ? (
                        <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-green-100 text-green-800">
                          Passed
                        </span>
                      ) : (
                        <>
                          <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-red-100 text-red-800">
                            Failed
                          </span>
                          <br />
                          {assesment.failureReasons.map((r) => (
                            <span className="mr-1 mt-1 inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-red-100 text-red-800">
                              {r}
                            </span>
                          ))}
                        </>
                      )}
                    </p>
                    <div className="border-t border-gray-200 px-4 py-5 sm:px-6">
                      <dl className="grid grid-cols-1 gap-x-4 gap-y-8 sm:grid-cols-2">
                        <div className="sm:col-span-1">
                          <dt className="text-sm font-medium text-gray-500">
                            Address Score
                          </dt>
                          <dd className="mt-1 text-sm text-gray-900">
                            {assesment.scoreAddress}
                          </dd>
                        </div>
                        <div className="sm:col-span-1">
                          <dt className="text-sm font-medium text-gray-500">
                            Address count
                          </dt>
                          <dd className="mt-1 text-sm text-gray-900">
                            {assesment.countAddresss}
                          </dd>
                        </div>
                        <div className="sm:col-span-1">
                          <dt className="text-sm font-medium text-gray-500">
                            Name Score
                          </dt>
                          <dd className="mt-1 text-sm text-gray-900">
                            {assesment.scoreName}
                          </dd>
                        </div>
                        <div className="sm:col-span-1">
                          <dt className="text-sm font-medium text-gray-500">
                            Name Count
                          </dt>
                          <dd className="mt-1 text-sm text-gray-900">
                            {assesment.countName}
                          </dd>
                        </div>
                        <div className="sm:col-span-1">
                          <dt className="text-sm font-medium text-gray-500">
                            Phone Score
                          </dt>
                          <dd className="mt-1 text-sm text-gray-900">
                            {assesment.scorePhone}
                          </dd>
                        </div>
                        <div className="sm:col-span-1">
                          <dt className="text-sm font-medium text-gray-500">
                            Phone Count
                          </dt>
                          <dd className="mt-1 text-sm text-gray-900">
                            {assesment.countPhone}
                          </dd>
                        </div>
                        <div className="sm:col-span-1">
                          <dt className="text-sm font-medium text-gray-500">
                            SSN Score
                          </dt>
                          <dd className="mt-1 text-sm text-gray-900">
                            {assesment.scoreSSN}
                          </dd>
                        </div>
                        <div className="sm:col-span-1">
                          <dt className="text-sm font-medium text-gray-500">
                            SSN Count
                          </dt>
                          <dd className="mt-1 text-sm text-gray-900">
                            {assesment.countSSN}
                          </dd>
                        </div>
                        <div className="sm:col-span-2">
                          <dt className="text-sm font-medium text-gray-500">
                            Address list
                          </dt>
                          <dd className="mt-1 text-sm text-gray-900">
                            {assesment.sourceAddress.map((addr) => (
                              <div key={addr[0]}>
                                <span className="inline-flex items-center mr-2 px-2.5 py-0.5 rounded-full text-xs font-medium bg-gray-100 text-gray-800">
                                  Score: {addr[1]}
                                </span>
                                {addr[0]}
                              </div>
                            ))}
                          </dd>
                        </div>
                        <div className="sm:col-span-2">
                          <dt className="text-sm font-medium text-gray-500">
                            Name list
                          </dt>
                          <dd className="mt-1 text-sm text-gray-900">
                            {assesment.sourceName.map((addr) => (
                              <div key={addr[0]}>
                                <span className="inline-flex items-center mr-2 px-2.5 py-0.5 rounded-full text-xs font-medium bg-gray-100 text-gray-800">
                                  Score: {addr[1]}
                                </span>
                                {addr[0]}
                              </div>
                            ))}
                          </dd>
                        </div>
                        <div className="sm:col-span-2">
                          <dt className="text-sm font-medium text-gray-500">
                            Phone list
                          </dt>
                          <dd className="mt-1 text-sm text-gray-900">
                            {assesment.sourcePhone.map((addr) => (
                              <div key={addr[0]}>
                                <span className="inline-flex items-center mr-2 px-2.5 py-0.5 rounded-full text-xs font-medium bg-gray-100 text-gray-800">
                                  Score: {addr[1]}
                                </span>
                                {addr[0]}
                              </div>
                            ))}
                          </dd>
                        </div>
                      </dl>
                    </div>
                  </div>
                </div>
              </section>
            ))}
          </div>
        </div>
      </div>
    );
  }

  return <div>Loading KYC</div>;
}
