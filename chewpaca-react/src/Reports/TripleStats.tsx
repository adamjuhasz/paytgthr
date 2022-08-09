interface Stat {
  name: string;
  stat: string;
}

interface Props {
  stats: Stat[];
  count?: number;
  color?: string;
}

export default function TripeStats({
  stats,
  count = 3,
  color = "indigo",
}: Props): JSX.Element {
  return (
    <div>
      <dl
        className={`mt-5 grid grid-cols-1 rounded-lg bg-white overflow-hidden shadow divide-y divide-gray-200 md:grid-cols-${count} md:divide-y-0 md:divide-x`}
      >
        {stats.map((item) => (
          <div key={item.name} className="px-4 py-5 sm:p-6">
            <dt className="text-base font-normal text-gray-900">{item.name}</dt>
            <dd className="mt-1 flex justify-between items-baseline md:block lg:flex">
              <div
                className={`flex items-baseline text-2xl font-semibold text-${color}-600`}
              >
                {item.stat}
              </div>
            </dd>
          </div>
        ))}
      </dl>
    </div>
  );
}
