export interface UserModel {
  user: {
    userid: string;
    firstName: string;
    lastName: string;
    email: string;
  };
  'fname-value': string | null;
  'lname-value': string | null;
  'dob-value': string | null;
  'phone-value': string | null;
  'street-value': string | null;
  'street2-value': string | null;
  'city-value': string | null;
  'state-value': string | null;
  'zip-value': string | null;
}
