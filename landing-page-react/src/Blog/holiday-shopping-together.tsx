import { Post } from "./Types";
import MikePhoto from "../LandingPage/PeterPerez.jpeg";
import { BlogContent, H3, P } from "./BlogContent";

export const title = "Getting your home holiday ready";
export const tag = "Couples Finances";
export const slug = "getting-your-home-holiday-ready-tgthr";
export const description =
  "Whether you’re hosting this year’s festivities or just cozying up for the winter, Pay Tgthr has some ideas to make the rooms in your home more festive, fun and inviting!";
export const photoid = "photo-1545024602-5ac8042e0bc5";

export const post: Post = {
  title,
  href: `/blog/${slug}`,
  category: { name: tag },
  description,
  date: "Nov 19, 2021",
  datetime: "2021-11-19",
  imageUrl: `https://images.unsplash.com/${photoid}?ixlib=rb-1.2.1&auto=format&fit=crop&w=1567&q=80`,
  imageAlt: "Two dogs looking at snow outside",
  readingTime: "3 min",
  author: {
    name: "Mike Perez, CEO",
    href: "#",
    imageUrl: MikePhoto,
  },
  reactNode: TheBlogPost,
};

export default post;

function TheBlogPost() {
  return (
    <BlogContent post={post}>
      <P>
        As Thanksgiving draws near, the faint whisper of Mariah Carey’s{" "}
        <a href="https://www.youtube.com/watch?v=aAkMkVFwAoo&ab_channel=MariahCareyVEVO">
          All I Want For Christmas
        </a>
        ” grows ever louder. You know what that means: it’s time to start
        prepping your home for the holiday season!
      </P>
      <P>
        Whether you’re hosting this year’s festivities or just cozying up for
        the winter, Pay Tgthr has some ideas to make the rooms in your home more
        festive, fun and inviting!
      </P>
      <H3>Warm vibes from the living room</H3>
      <P>
        For many of us, the living room is where we spend most of our time at
        home. Whether it’s a quiet night with your partner on the couch or
        hosting lively conversation with friends and family, the living room’s
        where it happens.
      </P>
      <P>
        What better way to make the room more inviting (and fragrant!) than a
        holiday themed scented candle. Mike is particularly fond of{" "}
        <a href="https://boysmells.com/collections/candles/holiday">
          Boy Smells
        </a>{" "}
        holiday candle collection. The warm glow of a candle paired with the
        smell of cedar or juniper wafting in the air can make any home{" "}
        <a href="https://en.wikipedia.org/wiki/Hygge">hygge</a>-ready for the
        season.
      </P>
      <P>
        Looking for some inviting tchotchkes to really revel in the season? Try
        adding some sleek, modern{" "}
        <a href="https://www.westelm.com/products/white-lacquer-snowman-figurines-d12873/?pkey=choliday-pillows-decor">
          snowmen
        </a>
        , a{" "}
        <a href="https://www.anthropologie.com/shop/nathalie-lete-holiday-village-house2?category=all-gifts&color=045&type=STANDARD&size=M&quantity=1">
          winter village
        </a>{" "}
        or some{" "}
        <a href="https://www.roomandboard.com/catalog/home-decor/holiday-decor/glint-pinecone-candles">
          faux pine cones
        </a>{" "}
        to your decor to bring the season to life indoors. And comfy up that
        couch with a{" "}
        <a href="https://www.gilt.com/boutique/product/159665/101470685/?deeplink=FALSE&subid=1396648518658&gclid=CjwKCAiAs92MBhAXEiwAXTi251FPRBH6H_BCaCRZN_Hx1z2ACrUplUp4Ht3NOGKsjYuYyUuvsbL3eRoCNBgQAvD_BwE&keyword=30300260010000&adposition=&partner=google&campaignid=13726115490&adgroupid=124471365979&country=US&matchtype=&network=u&device=c&currency=USD&dsi=DIR--00000000-0000-0000-0000-000000000000">
          flannel throw blanket
        </a>{" "}
        and fun{" "}
        <a href="https://www.urbanoutfitters.com/shop/tree-buddy-shaped-throw-pillow?category=apartment-room-decor&color=030&type=REGULAR&size=ONE%20SIZE&quantity=1">
          seasonal pillow
        </a>{" "}
        for a touch of warmth and fun.
      </P>
      <H3>Cooking for many, or just two </H3>
      <P>
        Holiday host duties can be exhausting. There’s dinner to be made, drinks
        to be poured and desserts to be served. But even if you won’t be
        hosting, your two should enjoy the holiday spirit every time you go to
        the kitchen!
      </P>
      <P>
        Holiday-themed{" "}
        <a href="https://www.potterybarn.com/products/hanukkah-star-salad-plates/?pkey=choliday-all-hanukkah-decor">
          dinnerware
        </a>
        ,{" "}
        <a href="https://www.bedbathandbeyond.com/store/product/lenox-holiday-balloon-wine-glasses-set-of-4/1044138912?keyword=holiday-wine-glasses">
          glassware
        </a>
        , and{" "}
        <a href="https://www.neimanmarcus.com/p/juliska-merry-christmas-gift-tray-prod242540375?utm_source=google_shopping&ecid=NMCS_GP_NCXX_XXXX_XXXXXXXXXXXXXXXX_XXXXXXXX&gclid=CjwKCAiAs92MBhAXEiwAXTi25_mjzSl4D7r4R2B0Xd21qcCaMx7mWVnlFfpRwbBz6c59meJTl-jjrhoCEd4QAvD_BwE&gclsrc=aw.ds">
          serving plates
        </a>{" "}
        all add a touch of spirit and fun to every meal you serve. Throw in some
        fun{" "}
        <a href="https://www.potterybarn.com/products/holiday-sentiment-tea-towels/?catalogId=84&sku=9365825&cm_ven=PLA&cm_cat=Google&cm_pla=Kitchen%20%26%20Dining%20%3E%20Kitchen%20Towels&region_id=820120&cm_ite=9365825&gclid=CjwKCAiAs92MBhAXEiwAXTi25-1AJgF30W_-2L_uE5kP646RL0SjeoRUwANEG120U9ZO8aXDT9Ri9RoC7HAQAvD_BwE">
          dish towels
        </a>{" "}
        and a festive{" "}
        <a href="https://www.williams-sonoma.com/products/greens-and-gourds-thanksgiving-centerpiece/?catalogId=79&sku=3805242&cm_ven=PLA&cm_cat=Google&cm_pla=Outdoor%20%3E%20Wreaths%20%26%20Garlands&region_id=820120&cm_ite=3805242&gclid=CjwKCAiAs92MBhAXEiwAXTi25-Cj4BRBdXi1wwuqY7p9rz3iGmxNiR-azkDaIrs-6oekYn4ejAmfkRoCACIQAvD_BwE">
          centerpiece
        </a>
        , and you two are all ready for a party of five or a dinner for two.{" "}
      </P>
      <P>
        No matter if the weather outside is frightful, your little corner of the
        world will be warm and inviting this winter. Don’t forget to take some
        time out of the hectic holiday season to spend quality time tgthr.
      </P>
    </BlogContent>
  );
}
