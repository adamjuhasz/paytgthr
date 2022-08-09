/* This example requires Tailwind CSS v2.0+ */
import { PropsWithChildren } from "react";
import { Disclosure } from "@headlessui/react";
import {
  GiftIcon,
  HomeIcon,
  LibraryIcon,
  MapIcon,
  PaperClipIcon,
  TrendingUpIcon,
  UsersIcon,
} from "@heroicons/react/outline";
import { NavLink } from "react-router-dom";

const navigation = [
  { name: "Home", icon: HomeIcon, href: "/" },
  {
    name: "Users",
    icon: UsersIcon,
    children: [
      { name: "Signed up today", href: "/users/today" },
      { name: "Search", href: "/users/search" },
      { name: "Browse", href: "/users/browse" },
    ],
  },
  {
    name: "App Events",
    icon: MapIcon,
    children: [
      { name: "Devices", href: "/appevents/devices" },
      { name: "Users", href: "/appevents/users" },
    ],
  },
  {
    name: "Rewards",
    icon: GiftIcon,
    children: [
      { name: "List all", href: "/rewards/list" },
      { name: "Create new", href: "/rewards/new" },
    ],
  },
  {
    name: "Journals",
    icon: PaperClipIcon,
    children: [
      { name: "List all", href: "/ledger/journals" },
      { name: "Stress test", href: "/ledger/test/stress" },
    ],
  },
  {
    name: "Referrals",
    icon: TrendingUpIcon,
    children: [
      { name: "All programs", href: "/referrals/programs" },
      { name: "New program", href: "/referrals/new/program" },
      { name: "All progress", href: "/referrals/progress" },
      { name: "All codes", href: "/referrals/codes" },
    ],
  },
  {
    name: "Payments",
    icon: LibraryIcon,
    children: [
      { name: "All payments", href: "/payments" },
      { name: "Scheduled payments", href: "/payments/scheduled" },
    ],
  },
  {
    name: "Invites",
    icon: LibraryIcon,
    children: [{ name: "All Invites", href: "/invites" }],
  },
  {
    name: "Reports",
    icon: LibraryIcon,
    children: [
      { name: "Churn", href: "/reports/churn" },
      { name: "Purchases", href: "/reports/purchases" },
      { name: "Users", href: "/reports/users" },
    ],
  },
];

interface NavProps {
  to: string;
  end?: boolean;
  className?: string;
}

const CustomNav = (props: PropsWithChildren<NavProps>) => (
  <NavLink
    to={props.to}
    end={props.end}
    className={({ isActive }) =>
      `bg-white text-gray-600 hover:bg-gray-100 hover:text-gray-900 group w-full flex items-center pl-2 pr-4 py-2 text-sm font-medium rounded-md ${
        isActive ? "bg-gray-200 text-gray-900 hover:bg-gray-300" : ""
      } ${props.className !== undefined ? props.className : ""}`
    }
  >
    {props.children}
  </NavLink>
);

function classNames(...classes: unknown[]) {
  return classes.filter(Boolean).join(" ");
}

export default function Example(): JSX.Element {
  return (
    <div className="flex flex-shrink-0 h-full">
      <div className="flex flex-col flex-grow border-r border-gray-200 pt-5 pb-4 bg-white overflow-y-auto">
        <div className="flex items-center flex-shrink-0 px-4">
          <img
            className="h-8 w-auto"
            src="https://tailwindui.com/img/logos/workflow-logo-indigo-600-mark-gray-800-text.svg"
            alt="Workflow"
          />
        </div>
        <div className="mt-5 flex-grow flex flex-col">
          <nav className="flex-1 px-2 space-y-1 bg-white" aria-label="Sidebar">
            {navigation.map((item) =>
              !item.children ? (
                <div key={item.name}>
                  <CustomNav to={item.href}>
                    <item.icon
                      className={classNames(
                        window.location.pathname.startsWith(item.href)
                          ? "text-gray-500"
                          : "text-gray-400 group-hover:text-gray-500",
                        "mr-3 flex-shrink-0 h-6 w-6"
                      )}
                      aria-hidden="true"
                    />
                    {item.name}
                  </CustomNav>
                </div>
              ) : (
                <Disclosure
                  as="div"
                  key={item.name}
                  className="space-y-1"
                  defaultOpen={true}
                >
                  {({ open }) => (
                    <>
                      <Disclosure.Button
                        className={classNames(
                          "bg-white text-gray-600 hover:bg-gray-100 hover:text-gray-900",
                          "group w-full flex items-center pl-2 pr-1 py-2 text-left text-sm font-medium rounded-md focus:outline-none focus:ring-2 focus:ring-indigo-500"
                        )}
                      >
                        <item.icon
                          className="mr-3 flex-shrink-0 h-6 w-6 text-gray-400 group-hover:text-gray-500"
                          aria-hidden="true"
                        />
                        <span className="flex-1">{item.name}</span>
                        <svg
                          className={classNames(
                            open ? "text-gray-400 rotate-90" : "text-gray-300",
                            "ml-3 flex-shrink-0 h-5 w-5 transform group-hover:text-gray-400 transition-colors ease-in-out duration-150"
                          )}
                          viewBox="0 0 20 20"
                          aria-hidden="true"
                        >
                          <path d="M6 6L14 10L6 14V6Z" fill="currentColor" />
                        </svg>
                      </Disclosure.Button>
                      <Disclosure.Panel className="space-y-1">
                        {item.children.map((subItem) => (
                          <Disclosure.Button
                            key={subItem.name}
                            as={CustomNav}
                            to={subItem.href}
                            className="pl-12"
                          >
                            {subItem.name}
                          </Disclosure.Button>
                        ))}
                      </Disclosure.Panel>
                    </>
                  )}
                </Disclosure>
              )
            )}
          </nav>
        </div>
      </div>
    </div>
  );
}
