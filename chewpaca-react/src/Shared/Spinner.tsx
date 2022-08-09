export default function Spinner(): JSX.Element {
  return (
    <div className="flex justify-center items-center px-4 py-0 text-sm font-medium shadow-sm text-white ">
      <div className="animate-spin rounded-full h-4 w-4 border-b-2 border-white "></div>
    </div>
  );
}
