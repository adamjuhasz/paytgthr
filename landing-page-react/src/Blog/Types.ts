export interface Post {
  title: string;
  href: string;
  category: {
    name:
      | "Couples Finances"
      | "Founders’ Forum"
      | "Announcement"
      | "Behind the scenes";
  };
  description: string;
  date: string;
  datetime: string;
  imageUrl: string;
  imageAlt: string;
  readingTime: string;
  author: {
    name: string;
    href: string;
    imageUrl: string;
  };
  visble?: boolean;
  reactNode: () => JSX.Element;
}
