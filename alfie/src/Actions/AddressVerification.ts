import { TenSeconds, fetchUA, retryFetch } from "./fetchRequests";

export interface Address {
  street: string;
  apt: string;
  city: string;
  state: string;
  zip: string;
}

interface SmartStreetResponse {
  input_index: number;
  candidate_index: number;
  delivery_line_1: string;
  delivery_line_2: string | undefined;
  last_line: string;
  delivery_point_barcode: string;
  components: {
    primary_number: string;
    street_name: string;
    street_suffix: string;
    city_name: string;
    state_abbreviation: string;
    zipcode: string;
    plus4_code: string;
    delivery_point: string;
    delivery_point_check_digit: string;
  };
  metadata: {
    record_type: string;
    zip_type: string;
    county_fips: string;
    county_name: string;
    carrier_route: string;
    congressional_district: string;
    rdi: string;
    elot_sequence: string;
    elot_sort: string;
    latitude: number;
    longitude: number;
    precision: string;
    time_zone: string;
    utc_offset: number;
    dst: boolean;
  };
  analysis: {
    dpv_match_code: string;
    dpv_footnotes: string;
    dpv_cmra: string;
    dpv_vacant: string;
    active: string;
    footnotes: string;
  };
}

export const verifyAddress = async (addr: Address): Promise<Address> => {
  const url = `https://us-street.api.smartystreets.com/street-address?street=${encodeURIComponent(
    addr.street
  )}&secondary=${encodeURIComponent(addr.apt)}&city=${encodeURIComponent(
    addr.city
  )}&state=${encodeURIComponent(addr.state)}&zipcode=${encodeURIComponent(
    addr.zip
  )}&key=12345&match=strict&candidates=1`;
  const res = await retryFetch(TenSeconds, url, {
    method: "GET",
    headers: {
      referer: "https://paytgthr.com",
      "User-Agent": fetchUA,
      Accept: "application/json",
    },
  });

  switch (res.status) {
    case 200: {
      const body = (await res.json()) as SmartStreetResponse[];
      if (body.length === 0) {
        throw new Error("Unknown address");
      }
      const suggested = body[0];
      const modified: Address = {
        street: suggested.delivery_line_1,
        apt:
          suggested.delivery_line_2 !== undefined
            ? suggested.delivery_line_2
            : "",
        city: suggested.components.city_name,
        state: suggested.components.state_abbreviation,
        zip: suggested.components.zipcode,
      };

      return modified;
    }

    default:
      throw new Error("not a 200");
  }
};
