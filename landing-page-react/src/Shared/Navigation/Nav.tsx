import { Link, NavLink } from "react-router-dom";

/* This example requires Tailwind CSS v2.0+ */
const navigation = [
  { name: "Cash back rewards", href: "/web/cashback" },
  { name: "Better Tgthr", href: "/blog" },
  { name: "In the press", href: "/web/press" },
];

export default function NavBar() {
  return (
    <header className="bg-rose-600">
      <nav className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8" aria-label="Top">
        <div className="hidden lg:flex w-full py-6  items-center justify-between border-b border-rose-500 lg:border-none">
          <div className="flex items-center">
            <Link to="/">
              <svg
                className="fill-current text-white h-7 w-auto"
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
            </Link>
            <div className="hidden ml-10 space-x-8 lg:block">
              <NavLink
                to="/"
                className="text-base font-medium text-white hover:text-rose-50 hover:bg-opacity-50 hover:bg-rose-500 p-2 rounded-lg"
              >
                Tgthr Card
              </NavLink>
              {navigation.map((link) => (
                <NavLink
                  key={link.name}
                  to={link.href}
                  className="text-base font-medium text-white hover:text-rose-50 hover:bg-opacity-50 hover:bg-rose-500 p-2 rounded-lg"
                  activeClassName="font-medium bg-rose-500 hover:bg-opacity-100"
                >
                  {link.name}
                </NavLink>
              ))}
            </div>
          </div>
        </div>
        <div className="pt-4 flex flex-wrap justify-center lg:hidden border-b border-rose-500">
          <NavLink
            to="/"
            className="text-base font-medium text-white hover:text-rose-50 hover:bg-opacity-50 hover:bg-rose-500 p-2 rounded-lg"
          >
            Tgthr Card
          </NavLink>
          {navigation.map((link) => (
            <NavLink
              key={link.name}
              to={link.href}
              className="text-base font-medium text-white hover:text-rose-50 p-2 rounded-lg"
              activeClassName="font-medium bg-rose-500"
            >
              {link.name}
            </NavLink>
          ))}
        </div>
        <div className="pb-4 flex flex-wrap justify-center lg:hidden">
          <a
            href="https://apps.apple.com/app/apple-store/id1463926588?pt=1164610&ct=LandingPage&mt=8"
            className="text-base font-medium text-white hover:text-rose-50 p-2 rounded-lg"
          >
            App Store
          </a>
          <a
            href="https://play.google.com/store/apps/details?id=com.paytgthr.alfie&utm_source=landingpage&utm_campaign=hero&pcampaignid=pcampaignidMKT-Other-global-all-co-prtnr-py-PartBadge-Mar2515-1"
            className="text-base font-medium text-white hover:text-rose-50 p-2 rounded-lg"
          >
            Play Store
          </a>
        </div>
      </nav>
    </header>
  );
}
