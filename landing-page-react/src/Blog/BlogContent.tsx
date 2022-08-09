import { PropsWithChildren, ReactNode } from "react";
import { Helmet } from "react-helmet";
import { Post } from "./Types";

interface Props {
  post: Post;
  intro?: ReactNode;
}

export const P = (props: PropsWithChildren<unknown>) => (
  <p className="mt-8 text-lg text-gray-500">{props.children}</p>
);

export const H3 = (props: PropsWithChildren<unknown>) => (
  <h3>{props.children}</h3>
);

export function BlogContent(props: PropsWithChildren<Props>) {
  return (
    <>
      <Helmet>
        <title>{props.post.title}</title>
        <meta content={props.post.description} name="Description" />
      </Helmet>
      <div className="relative bg-white">
        <div className="lg:absolute lg:inset-0">
          <div className="lg:absolute  lg:inset-y-0 lg:left-0 lg:w-1/2">
            <img
              className="h-56 w-full object-cover lg:absolute lg:h-full"
              src={props.post.imageUrl}
              alt={props.post.imageAlt}
            />
          </div>
        </div>
        <div className="relative pt-12 pb-16 px-4 sm:pt-16 sm:px-6 lg:px-8 lg:max-w-7xl lg:mx-auto lg:grid lg:grid-cols-2">
          <div className="lg:col-start-2 lg:pl-8">
            <div className="text-base max-w-prose mx-auto lg:max-w-lg lg:ml-auto lg:mr-0">
              <h2 className="leading-6 text-rose-600 font-semibold tracking-wide uppercase">
                {props.post.category.name}
              </h2>
              <h1 className="mt-2 text-3xl leading-8 font-extrabold tracking-tight text-gray-900 sm:text-4xl">
                {props.post.title}
              </h1>
              {props.intro === undefined ? (
                <></>
              ) : (
                <p className="mt-8 text-lg text-gray-500">{props.intro}</p>
              )}
              <div className="mt-5 prose prose-rose text-gray-500">
                {props.children}
              </div>
            </div>
          </div>
        </div>
      </div>
    </>
  );
}
