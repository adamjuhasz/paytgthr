/* This example requires Tailwind CSS v2.0+ */

import Destiny from "./Testinomials/Destiny";
import Joey from "./Testinomials/Joey";
import Thomas from "./Testinomials/Thomas";
import Vikas from "./Testinomials/Vikas";

export default function Example() {
  return (
    <section className="bg-rose-600">
      <div className="max-w-7xl mx-auto md:grid md:grid-cols-2 md:px-6 lg:px-8">
        <Destiny />
        <Joey />
      </div>

      <div className="max-w-7xl mx-auto md:grid md:grid-cols-2 md:px-6 lg:px-8">
        <Thomas />
        <Vikas />
      </div>
    </section>
  );
}
