import { Helmet } from "react-helmet";
import { Post } from "./Types";
import AdamPhoto from "../LandingPage/AdamJuhasz.png";

export const slug = "securing-a-fintech-security-at-pay-tgthr";
export const photoid = "photo-1514302240736-b1fee5985889"; // photo-1603793354938-6b393b09a688 or photo-1563920443079-783e5c786b83

export const post: Post = {
  title: "How to secure a fintech",
  href: `/blog/${slug}`,
  category: { name: "Behind the scenes" },
  description: "How to secure a fintech from external security threats",
  date: "Nov 28, 2021",
  datetime: "2021-11-28",
  imageUrl: `https://images.unsplash.com/${photoid}?ixlib=rb-1.2.1&auto=format&fit=crop&w=1567&q=80`,
  imageAlt: "A stormtrooper securing the Gibson",
  readingTime: "5 min",
  author: {
    name: "Adam Juhasz, CTO",
    href: "#",
    imageUrl: AdamPhoto,
  },
  visble: false,
  reactNode: BlogContent,
};

export default post;

function BlogContent() {
  return (
    <>
      <Helmet>
        <title>{post.title}</title>
        <meta content={post.description} name="Description" />
      </Helmet>
      <div className="relative bg-white">
        <div className="lg:absolute lg:inset-0">
          <div className="lg:absolute lg:inset-y-0 lg:left-0 lg:w-1/2">
            <img
              className="h-56 w-full object-cover lg:absolute lg:h-full"
              src={`https://images.unsplash.com/${photoid}?ixlib=rb-1.2.1&auto=format&fit=crop&w=1567&q=80`}
              alt={post.imageAlt}
            />
          </div>
        </div>
        <div className="relative pt-12 pb-16 px-4 sm:pt-16 sm:px-6 lg:px-8 lg:max-w-7xl lg:mx-auto lg:grid lg:grid-cols-2">
          <div className="lg:col-start-2 lg:pl-8">
            <div className="text-base max-w-prose mx-auto lg:max-w-lg lg:ml-auto lg:mr-0">
              <h2 className="leading-6 text-rose-600 font-semibold tracking-wide uppercase">
                {post.category.name}
              </h2>
              <h1 className="mt-2 text-3xl leading-8 font-extrabold tracking-tight text-gray-900 sm:text-4xl">
                {post.title}
              </h1>
              <div className="mt-5 prose prose-rose text-gray-500">
                <p>
                  At Pay Tgthr we take the responsibility of keeping our users'
                  data and money secure and safe very seriously. We wanted to
                  share how we keep external threats at bay, we'll cover
                  security around internal threats in a seperate article.
                </p>

                <h2>Architecting for security</h2>
                <p>
                  We started thinking about security before the very fist line
                  of code was written by making sure we hardened our system
                  architecture first. The architecture of a system is like the
                  blueprint of an villain's base. You have to decide where to
                  put the base and what type of moat and fence you'll have.
                </p>

                <p>
                  On the application side we have a 2 layer onion architecture.
                  Like with an onion only the outer layer touches the outside
                  world (the internet here). We develop sepcific applications
                  that talk to our external partners and 1 application talks
                  only to 1 specific partner. This afforts us a few benefits:
                </p>
                <ul>
                  <li>
                    Our first layer has no access to customer information or any
                    permanent storage like a database or file syste,. It can
                    only act as a bridge from an external partner to the inner
                    layer of the onion.
                  </li>
                  <li>
                    Keeps our code short and easy to understand. Each line of
                    code we write is another opportunity to make a mistake a
                    hacker can take advantage of. The less we write the better.
                  </li>
                </ul>

                <h2>Locating your servers</h2>
                <p>
                  Like many fintechs we host our systems in the cloud instead of
                  on our own servers. We made this decision because it lets us
                  focus our limited resources on the security of our services
                  instead of lower-level security like keeping our machines and
                  other devices up to date and even physcial secuirty like
                  making sure no one walks out of the data center with our
                  computers. Google Cloud takes care of automatically keeping
                  our network and machine infrastrcutre up to date and secure as
                  well as physical security.
                </p>

                {/* <h2>Making sure the fence and entry is strong</h2> */}

                <h2>Securing personal information</h2>
                <p>
                  There are two places personal information can be accessed,
                  when it is "at rest", sitting in a databse, and when it is "in
                  transit", when two services are exchanging information.
                </p>
                <h4>Securing data at rest</h4>
                <p>
                  At the lowest level, actually storing data on disks inside our
                  server each hard drive is encrypted. This means that physical
                  theft of our servers would not allow an attacker to gain
                  access to the information stored inside.
                </p>
                <p>
                  At the application layer we store data in our databases both
                  in plain text, meaning it is human and machine redable and
                  encrypted. This means that even if an attacker was able to
                  breach our platform and access our databasse they would still
                  only see encrypted gibberesh. We store things like passwords,
                  social security numbers (
                  <abbr title="Social Security Number">SSN</abbr>), bank account
                  details, and other sensitive personal information (
                  <abbr title="Personal Identifiable Information">PII</abbr>)
                  encrypted in the databse. Every time we choose to store a new
                  piece of information our team ranks decides if it should be
                  encrypted based the following factors:
                </p>
                <ul>
                  <li>
                    Would exposure of this information cause harm directly or
                    indirectly to our customer? If yes, ecrypot it at the
                    application layer. <i>Example: SSN due to identity theft</i>
                  </li>
                  <li>
                    Does more than 1 service in the platform need access? If no,
                    encrypt it at the application layer.
                  </li>
                  <li>
                    Do our users consider this information personal? If yes,
                    ecnrypt it at the application layer.
                  </li>
                  <li>
                    Would exposing this information compromise the security of
                    the platform? If yes, encrypt it at the application layer.{" "}
                    <i>
                      Example: Passwords, a leak would allow unuthroized access
                      to customer accounts
                    </i>
                  </li>
                </ul>
                <h4>Securing data in transit</h4>
                <p>
                  Many of our services are required to exhange sentitive
                  information, for example we need to send customers' SSNs to
                  our identity check partner to verify theit identities. This
                  means transferrinf it within our platform from the accounts
                  service to the bridge to our identity verification partner.
                  While the data is being transmitted we both encrypt it, using
                  TLS 1.3, so no one can intercept the information and have both
                  the transmitter and reciever first verify their own
                  identities, using mTLS, so that neither end can be
                  impersonated.
                </p>
                <p>
                  At the lowest level, actually storing data on disks inside our
                  server each hard drive is encrypted. This means that physical
                  theft of our servers would not allow an attacker to gain
                  access to the information stored inside.
                </p>

                <h2>Keeping track of whats happening</h2>
                <p>
                  We monitor both what's changed and what was accessed. This
                  means we can keep track of what changes have been made to our
                  platform, for example a customer changed their phone number.
                  We keep track track of each version of customers' data. This
                  allows us to "undo" anything in case of a mistake or malicious
                  change. We also keep track of all access to customer data.
                  With this we can notice if the rate of access is increasing.
                  If all of a sudden a lot more data is being accessed we can be
                  alerted and investigate the source and reason.
                </p>
              </div>
            </div>
          </div>
        </div>
      </div>
    </>
  );
}
