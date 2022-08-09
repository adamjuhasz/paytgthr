import { useState } from "react";
import useGetNotes from "./UseGetNotes";
import useChangeNotes from "./UseChangeNotes";

interface Props {
  user: string;
}

export default function UserNotes(props: Props): JSX.Element {
  const [text, setText] = useState("");
  const notes = useGetNotes(props.user);
  const changeNote = useChangeNotes(props.user);

  if (notes.status === "success") {
    return (
      <div className="mt-1">
        <textarea
          rows={4}
          name="comment"
          id="comment"
          className="shadow-sm focus:ring-indigo-500 focus:border-indigo-500 block w-full sm:text-sm border-gray-300 rounded-md"
          value={text}
          onChange={(e) => {
            setText(e.target.value);
          }}
          placeholder={"Add new note here"}
        />
        <button
          disabled={changeNote.isLoading}
          onClick={async () => {
            await changeNote.mutateAsync(text);
            setText("");
          }}
          type="submit"
          className="mt-2 inline-flex items-center px-4 py-2 border border-transparent text-sm leading-5 font-medium rounded-md focus:outline-none transition ease-in-out duration-150  text-white bg-indigo-600 hover:bg-indigo-500 focus:border-indigo-700 focus:shadow-outline-indigo active:bg-indigo-700 "
        >
          Update
        </button>
        <textarea
          rows={notes.data.note.split(/\r\n|\r|\n/).length}
          value={notes.data.note}
          contentEditable={false}
          className="mt-2 shadow-sm focus:ring-indigo-500 focus:border-indigo-500 block w-full sm:text-sm border-gray-200 rounded-md text-gray-500"
          placeholder="No notes yet..."
        />
      </div>
    );
  }

  return <div>Loading notes</div>;
}
