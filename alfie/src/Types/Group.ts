export type GroupStates =
  | 'groupcreated'
  | 'grouppending'
  | 'groupactive'
  | 'grouppaused'
  | 'groupclosed'
  | 'groupexpired'
  | 'groupdenied';

export interface GroupModel {
  end: null | Date;
  id: string;
  members: [{accepted: boolean; user: string}];
  msgsource: string;
  revision: number;
  split: [
    {
      approved: boolean;
      ratio: {denominator: number; numerator: number};
      user: string;
    }
  ];
  start: null | Date;
  status: GroupStates;
  version: '1.0';
}
