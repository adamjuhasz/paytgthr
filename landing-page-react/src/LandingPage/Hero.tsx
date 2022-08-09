/* eslint-disable jsx-a11y/anchor-is-valid */
import { Link } from "react-router-dom";

import { ChevronRightIcon, StarIcon } from "@heroicons/react/solid";
import heroImage from "./HeroNoShadow-fs8.png";

import { posts } from "../Blog";

const Hero = () => {
  const visiblePosts = posts
    .filter((p) => p.visble !== false)
    .filter((p) => p.category.name === "Announcement");
  return (
    <div className="pt-8 overflow-hidden sm:pt-12 lg:relative lg:py-48">
      <div className="mx-auto max-w-md px-4 sm:max-w-3xl sm:px-6 lg:px-8 lg:max-w-7xl lg:grid lg:grid-cols-2 lg:gap-24">
        <div>
          <div>
            <svg
              className="fill-current text-rose-500 h-9 w-auto"
              width="945px"
              height="208px"
              viewBox="0 0 945 208"
              version="1.1"
              xmlns="http://www.w3.org/2000/svg"
            >
              <g id="Page-1" stroke="none" stroke-width="1">
                <g
                  id="White-Card"
                  transform="translate(-34.000000, -417.000000)"
                >
                  <path
                    d="M64.848,608.344 L64.848,553.016 C73.792,563 86.272,566.12 97.088,566.12 C127.248,566.12 148.256,545.528 148.256,511.832 C148.256,480.008 127.664,458.168 97.504,458.168 C84.4,458.168 72.752,461.288 63.392,473.56 L61.52,460.456 L34.688,460.456 L34.688,608.344 L64.848,608.344 Z M92.928,538.664 C78.992,538.664 65.264,529.72 65.264,511.832 C65.264,492.28 79.408,485.416 92.928,485.416 C108.944,485.416 118.096,497.064 118.096,511.832 C118.096,527.432 108.528,538.664 92.928,538.664 Z M215.024,566.12 C225.008,566.12 239.984,561.752 246.64,550.104 L248.304,563 L275.344,563 L275.344,460.248 L247.264,460.248 L246.224,472.104 C240.4,462.744 228.128,457.96 214.4,457.96 C184.864,457.96 161.984,478.136 161.984,511.624 C161.984,545.528 184.24,566.12 215.024,566.12 Z M218.976,538.664 C204,538.664 191.936,528.68 191.936,511.624 C191.936,494.568 204,484.376 218.976,484.376 C236.656,484.376 246.224,497.896 246.224,512.664 C246.224,525.352 235.824,538.664 218.976,538.664 Z M343.152,609.176 L406.176,460.248 L373.104,460.248 L347.936,524.936 L322.352,460.248 L288.656,460.248 L331.712,559.88 L310.496,609.176 L343.152,609.176 Z M519.12,566.12 C527.232,565.496 533.264,564.664 540.752,561.544 L533.888,536.584 C530.56,538.248 524.944,539.08 521.408,539.08 C515.792,539.08 509.76,536.168 509.76,525.352 L509.76,485.832 L535.968,485.832 L535.968,460.872 L509.968,460.872 L509.968,432.168 L480.016,435.496 L480.016,460.872 L461.712,460.872 L461.712,485.832 L480.016,485.832 L480.016,525.352 C480.016,557.384 496.864,567.16 519.12,566.12 Z M605.648,624.984 C633.728,624.984 661.392,610.216 661.392,579.64 C661.392,573.816 661.392,556.552 638.512,546.568 C654.112,538.456 660.56,524.104 660.56,508.504 C660.56,498.936 656.816,489.368 649.952,481.048 L663.056,465.448 L641.424,448.808 L630.192,463.368 C623.536,458.584 613.344,457.752 606.064,457.752 C577.36,457.752 550.528,475.432 550.528,510.168 C550.528,541.992 572.368,561.96 606.064,561.96 C624.576,561.96 631.024,569.656 631.024,579.64 C631.024,592.328 618.96,598.568 605.648,598.568 C591.712,598.568 580.896,593.576 579.856,577.768 L551.984,577.768 C551.36,607.512 569.872,624.984 605.648,624.984 Z M606.064,534.712 C590.88,534.712 579.44,525.144 579.44,510.168 C579.44,493.528 590.88,484.376 606.064,484.376 C620.832,484.376 631.856,495.816 631.856,510.168 C631.856,526.6 619.584,534.712 606.064,534.712 Z M732.32,566.12 C740.432,565.496 746.464,564.664 753.952,561.544 L747.088,536.584 C743.76,538.248 738.144,539.08 734.608,539.08 C728.992,539.08 722.96,536.168 722.96,525.352 L722.96,485.832 L749.168,485.832 L749.168,460.872 L723.168,460.872 L723.168,432.168 L693.216,435.496 L693.216,460.872 L674.912,460.872 L674.912,485.832 L693.216,485.832 L693.216,525.352 C693.216,557.384 710.064,567.16 732.32,566.12 Z M799.504,563 L799.504,510.584 C799.504,493.944 809.904,485.416 822.384,485.416 C834.24,485.416 841.936,493.32 841.936,507.672 L841.936,563 L871.888,563 L871.888,507.464 C871.888,470.856 855.04,458.168 830.912,458.168 C820.304,458.168 808.24,461.496 799.712,472.52 L799.712,417.4 L769.552,417.4 L769.552,563 L799.504,563 Z M925.76,563 L925.76,509.544 C925.76,494.152 936.784,485.832 949.68,485.832 C955.712,485.832 961.328,487.496 966.112,491.032 L978.592,467.32 C971.52,461.08 963.2,457.96 953.424,457.96 C941.776,457.96 932.832,461.912 925.968,471.688 L923.68,460.456 L895.808,460.456 L895.808,563 L925.76,563 Z"
                    id="paytgthr-black-text"
                  ></path>
                </g>
              </g>
            </svg>
          </div>
          <div className="mt-20 sm:mt-1">
            <div>
              <Link to={visiblePosts[0].href} className="inline-flex space-x-4">
                <span className="rounded bg-rose-50 px-2.5 py-1 text-xs font-semibold text-rose-500 tracking-wide uppercase">
                  What's new
                </span>
                <span className="inline-flex items-center text-sm font-medium text-rose-500 space-x-1">
                  <span>{visiblePosts[0].title}</span>
                  <ChevronRightIcon className="h-5 w-5" aria-hidden="true" />
                </span>
              </Link>
            </div>
            <div className="mt-6 mb-6 sm:max-w-xl">
              <h1 className="text-4xl font-extrabold text-gray-900 tracking-tight sm:text-5xl">
                Self driving money for couples
              </h1>
              <p className="mt-6 text-xl text-gray-500">
                Spend, save, and live better together. We replace your joint
                checking account, joint savings account, and money under the
                mattress with the{" "}
                <span className="font-medium">Tgthr Card</span>.
              </p>
              <p className="mt-6 text-xl text-gray-500">
                When either of your Tgthr Cards is used, we'll split your
                purchase between your individual checking accounts.
              </p>
            </div>
            <a href="https://apps.apple.com/app/apple-store/id1463926588?pt=1164610&ct=LandingPage&mt=8">
              <div className="flex mt-3 w-48 h-14 bg-black text-white rounded-xl items-center justify-center">
                <div className="mr-3">
                  <svg viewBox="0 0 384 512" width="30">
                    <path
                      fill="currentColor"
                      d="M318.7 268.7c-.2-36.7 16.4-64.4 50-84.8-18.8-26.9-47.2-41.7-84.7-44.6-35.5-2.8-74.3 20.7-88.5 20.7-15 0-49.4-19.7-76.4-19.7C63.3 141.2 4 184.8 4 273.5q0 39.3 14.4 81.2c12.8 36.7 59 126.7 107.2 125.2 25.2-.6 43-17.9 75.8-17.9 31.8 0 48.3 17.9 76.4 17.9 48.6-.7 90.4-82.5 102.6-119.3-65.2-30.7-61.7-90-61.7-91.9zm-56.6-164.2c27.3-32.4 24.8-61.9 24-72.5-24.1 1.4-52 16.4-67.9 34.9-17.5 19.8-27.8 44.3-25.6 71.9 26.1 2 49.9-11.4 69.5-34.3z"
                    />
                  </svg>
                </div>
                <div>
                  <div className="text-xs">Download on the</div>
                  <div className="text-2xl font-semibold font-sans -mt-1">
                    App Store
                  </div>
                </div>
              </div>
            </a>
            <a href="https://play.google.com/store/apps/details?id=com.paytgthr.alfie&utm_source=landingpage&utm_campaign=hero&pcampaignid=pcampaignidMKT-Other-global-all-co-prtnr-py-PartBadge-Mar2515-1">
              <div className="flex mt-3 w-48 h-14 bg-black text-white rounded-lg items-center justify-center">
                <div className="mr-3">
                  <svg viewBox="30 336.7 120.9 129.2" width="30">
                    <path
                      fill="#FFD400"
                      d="M119.2,421.2c15.3-8.4,27-14.8,28-15.3c3.2-1.7,6.5-6.2,0-9.7  c-2.1-1.1-13.4-7.3-28-15.3l-20.1,20.2L119.2,421.2z"
                    />
                    <path
                      fill="#FF3333"
                      d="M99.1,401.1l-64.2,64.7c1.5,0.2,3.2-0.2,5.2-1.3  c4.2-2.3,48.8-26.7,79.1-43.3L99.1,401.1L99.1,401.1z"
                    />
                    <path
                      fill="#48FF48"
                      d="M99.1,401.1l20.1-20.2c0,0-74.6-40.7-79.1-43.1  c-1.7-1-3.6-1.3-5.3-1L99.1,401.1z"
                    />
                    <path
                      fill="#3BCCFF"
                      d="M99.1,401.1l-64.3-64.3c-2.6,0.6-4.8,2.9-4.8,7.6  c0,7.5,0,107.5,0,113.8c0,4.3,1.7,7.4,4.9,7.7L99.1,401.1z"
                    />
                  </svg>
                </div>
                <div>
                  <div className="text-xs">GET IT ON</div>
                  <div className="text-xl font-semibold font-sans -mt-1">
                    Google Play
                  </div>
                </div>
              </div>
            </a>
            <div className="mt-6">
              <div className="inline-flex items-center divide-x divide-gray-300">
                <div className="flex-shrink-0 flex pr-5">
                  <StarIcon
                    className="h-5 w-5 text-yellow-400"
                    aria-hidden="true"
                  />
                  <StarIcon
                    className="h-5 w-5 text-yellow-400"
                    aria-hidden="true"
                  />
                  <StarIcon
                    className="h-5 w-5 text-yellow-400"
                    aria-hidden="true"
                  />
                  <StarIcon
                    className="h-5 w-5 text-yellow-400"
                    aria-hidden="true"
                  />
                  <StarIcon
                    className="h-5 w-5 text-yellow-400"
                    aria-hidden="true"
                  />
                </div>
                <div className="min-w-0 flex-1 pl-5 py-1 text-sm text-gray-500 sm:py-3">
                  <span className="font-medium text-gray-900">
                    Rated 4.4 stars
                  </span>{" "}
                  by over{" "}
                  <span className="font-medium text-rose-500">
                    200 happy users
                  </span>
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>

      <div className="sm:mx-auto sm:max-w-3xl sm:px-6">
        <div className="py-12 sm:relative sm:mt-12 sm:py-16 lg:absolute lg:inset-y-0 lg:right-0 lg:w-1/2">
          <div className="relative -mr-40 sm:mx-auto sm:max-w-3xl sm:px-0 lg:max-w-none lg:h-full">
            <img
              className="w-full rounded-md lg:h-full lg:w-auto lg:max-w-none"
              src={heroImage}
              alt="Screenshot of the Pay Tgthr app"
            />
          </div>
        </div>
      </div>
    </div>
  );
};

export default Hero;
