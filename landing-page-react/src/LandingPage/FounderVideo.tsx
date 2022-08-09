export default function FounderVideo() {
  return (
    <div className="max-w-3xl mx-auto mt-1 mb-8" id="founding-video">
      <div className="w-full text-center text-gray-500">
        Interview with our CEO Mike about Pay Tgthr's founding and future
      </div>
      <div className="aspect-w-16 aspect-h-9 shadow-xl">
        <iframe
          className="w-full h-full rounded-lg overflow-hidden"
          title="Pay Tgthr Founding Story, an interview with Mike Perez CEO"
          src="https://www.youtube-nocookie.com/embed/YrXqX8poZRk"
          frameBorder="0"
          allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture"
          allowFullScreen
        />
      </div>
    </div>
  );
}
