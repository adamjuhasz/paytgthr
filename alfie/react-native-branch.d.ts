/* eslint-disable @typescript-eslint/no-explicit-any */
/* eslint-disable @typescript-eslint/no-empty-interface */

declare module "react-native-branch" {
  export const VERSION: string;

  type AnyDataType =
    | string
    | boolean
    | number
    | null
    | undefined
    | AnyDataArray
    | AnyDataObject;
  interface AnyDataArray extends Array<AnyDataType> {}
  interface AnyDataObject extends Record<string, AnyDataType> {}

  export interface BranchBaseParams {
    "+clicked_branch_link": boolean;
    "+is_first_session": boolean;
    "+non_branch_link": undefined;
  }

  export interface NonBranchLinkParams extends BranchBaseParams {
    "+non_branch_link": string;
    "+clicked_branch_link": false;
  }

  export interface BranchLinkParams extends BranchBaseParams {
    $deeplink_path?: string;
    $marketing_title: string;
    $one_time_use?: boolean;
    "~channel"?: string;
    "~feature"?: string;
    "~tags"?: string[];
    "~campaign"?: string;
    "~stage"?: string;
    "~creation_source"?: string;
    "~referring_link"?: string;
    "~id"?: string;
    "+match_guaranteed": boolean;
    "+referrer"?: string;
    "+phone_number"?: string;
    "+url"?: string;
    "+rn_cached_initial_event"?: boolean;
    "+is_first_session": boolean;
    "~marketing"?: boolean;
    "+match_guaranteed"?: boolean;
    "+click_timestamp"?: number;
    "~creation_source"?: number;
  }

  type BranchEventParams = Pick<
    BranchEvent,
    | "transactionID"
    | "currency"
    | "revenue"
    | "shipping"
    | "tax"
    | "coupon"
    | "affiliation"
    | "description"
    | "searchQuery"
    | "alias"
    | "customData"
  >;

  export class BranchEvent {
    logEvent: () => Promise<null>;
    constructor(
      name: string,
      contentItems?: BranchUniversalObject | BranchUniversalObject[],
      params?: BranchEventParams
    );
    name: string;
    contentItems: BranchUniversalObject[];

    transactionID?: string;
    currency?: string;
    revenue?: string | number;
    shipping?: string | number;
    tax?: string | number;
    coupon?: string;
    affiliation?: string;
    description?: string;
    searchQuery?: string;
    alias?: string;
    customData?: Record<string, AnyDataType>;

    /**
     * Standard Add to Cart event
     */
    static AddToCart: string;

    /**
     * Standard Add to Wishlist event
     */
    static AddToWishlist: string;

    /**
     * Standard View Cart event
     */
    static ViewCart: string;

    /**
     * Standard Initiate Purchase event
     */
    static InitiatePurchase: string;

    /**
     * Standard Add Payment Info event
     */
    static AddPaymentInfo: string;

    /**
     * Standard Purchase event
     */
    static Purchase: string;

    /**
     * Standard Spend Credits event
     */
    static SpendCredits: string;

    /**
     * Standard View Ad event
     */
    static ViewAd: string;

    /**
     * Standard Click Ad event
     */
    static ClickAd: string;

    // Content events

    /**
     * Standard Search event
     */
    static Search: string;

    /**
     * Standard View Item event for a single Branch Universal Object
     */
    static ViewItem: string;

    /**
     * Standard View Items event for multiple Branch Universal Objects
     */
    static ViewItems: string;

    /**
     * Standard Rate event
     */
    static Rate: string;

    /**
     * Standard Share event
     */
    static Share: string;

    // User Lifecycle Events

    /**
     * Standard Complete Registration event
     */
    static CompleteRegistration: string;

    /**
     * Standard Complete Tutorial event
     */
    static CompleteTutorial: string;

    /**
     * Standard Achieve Level event
     */
    static AchieveLevel: string;

    /**
     * Standard Unlock Achievement event
     */
    static UnlockAchievement: string;

    /**
     * Standard Invite event
     */
    static Invite: string;

    /**
     * Standard Login event
     */
    static Login: string;

    /**
     * Standard Reserve event
     */
    static Reserve: string;

    /**
     * Standard Subscribe event
     */
    static Subscribe: string;

    /**
     * Standard Start Trial event
     */
    static StartTrial: string;
  }

  interface BranchSubscriptionEventBase {
    params: BranchLinkParams | NonBranchLinkParams | undefined;
    error: string | null | undefined;
    uri: string | undefined;
  }
  interface BranchSubscriptionEventError extends BranchSubscriptionEventBase {
    error: string;
    params: undefined;
    uri: undefined;
  }
  interface BranchSubscriptionEventSuccess extends BranchSubscriptionEventBase {
    error: null | undefined;
    params: BranchLinkParams | NonBranchLinkParams;
    uri: string;
  }

  export type BranchSubscriptionEvent =
    | BranchSubscriptionEventError
    | BranchSubscriptionEventSuccess;

  export interface BranchOpenStartEvent {
    uri: string;
    cachedInitialEvent?: boolean;
  }
  type BranchSubscribeCallback = (event: BranchSubscriptionEvent) => void;
  interface BranchSubscribeOptions {
    onOpenComplete: BranchSubscribeCallback;
    onOpenStart?: (event: BranchOpenStartEvent) => void;
  }
  type BranchUnsubscribe = () => void;
  type BranchSubscribe = (
    options: BranchSubscribeCallback | BranchSubscribeOptions
  ) => BranchUnsubscribe;

  interface BranchUniversalObjectOptions {
    locallyIndex?: boolean;
    publiclyIndex?: boolean;
    canonicalUrl?: string;
    title?: string;
    contentDescription?: string;
    contentImageUrl?: string;
    contentMetadata?: {
      price?: number | string;
      contentSchema?: any; // TODO
      quantity?: number;
      sku?: string;
      productName?: string;
      productBrand?: string;
      productCategory?: any; // TODO
      productVariant?: string;
      condition?: any; // TODO
      currency?: string;
      ratingAverage?: number;
      ratingCount?: number;
      ratingMax?: number;
      addressStreet?: string;
      addressCity?: string;
      addressRegion?: string;
      addressCountry?: string;
      addressPostalCode?: string;
      latitude?: number;
      longitude?: number;
      imageCaptions?: string[];
      customMetadata?: Record<string, string>;
    };
  }

  interface BranchShareSheetOptions {
    messageHeader?: string;
    messageBody?: string;
    emailSubject?: string;
    title?: string;
    text?: string;
  }

  interface BranchLinkProperties {
    alias?: string;
    campaign?: string;
    feature?: string;
    channel?: string;
    stage?: string;
    tags?: string[];
  }

  interface BranchLinkControlParams {
    $fallback_url?: string;
    $desktop_url?: string;
    $ios_url?: string;
    $ipad_url?: string;
    $android_url?: string;
    $samsung_url?: string;
  }

  interface BranchUniversalObject {
    ident: string;
    showShareSheet: (
      shareOptions?: BranchShareSheetOptions,
      linkProperties?: BranchLinkProperties,
      controlParams?: BranchLinkControlParams
    ) => Promise<{ channel: string; completed: string; error: any }>;
    generateShortUrl: (
      linkProperties: BranchLinkProperties,
      controlParams: BranchLinkControlParams
    ) => Promise<{ url: string }>;
    logEvent: (eventName: string, params?: BranchEventParams) => Promise<null>;
    release: () => void;
  }

  export interface Branch {
    subscribe: BranchSubscribe;
    skipCachedEvents: () => void;
    disableTracking: (disable: boolean) => void;
    isTrackingDisabled: boolean;
    getLatestReferringParams: (
      synchronous?: boolean
    ) => Promise<BranchLinkParams | NonBranchLinkParams>;
    getFirstReferringParams: () => Promise<
      BranchLinkParams | NonBranchLinkParams
    >;
    setIdentity: (identity: string) => void;
    setRequestMetadata: (key: string, value: string) => void;
    logout: () => void;
    openURL: (url: string, options?: { newActivity?: boolean }) => void;
    redeemRewards: (
      amount: number,
      bucket?: string
    ) => Promise<{ changed: boolean }>;
    loadRewards: (bucket?: string) => Promise<{ credits: number }>;
    getCreditHistory: () => Promise<any>; // TODO
    createBranchUniversalObject: (
      identifier: string,
      options: BranchUniversalObjectOptions
    ) => Promise<BranchUniversalObject>;
  }
  declare const branch: Branch;
  export default branch;
}
