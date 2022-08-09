import {QueryResult} from 'react-query';

export interface UnivitedState {
  type: 'uninvited';
  'partner-name'?: string;
  'partner-email'?: string;
  'partner-phone'?: string;
  'medium-value'?: InviteMediums;
  'medium-error'?: boolean;
  'firstname-error'?: boolean;
  'email-error'?: boolean;
  'phone-error'?: boolean;
  'selfinvite-error'?: boolean;
  'has-group'?: boolean;
}

export interface WaitingState {
  type: 'waiting';
  partner_name: string;
  group_id: string;
  partner_email: string;
}

export interface ConfirmState {
  type: 'confirm';
  partner_name: string;
  group_id: string;
}

export interface AcceptedState {
  type: 'accepted';
  partner_name: string;
  group_id: string;
}

export interface OverwriteState {
  type: 'overwrite';
  inviter_name: string;
  inviter_uid: string;
  group_id: string;
}

export interface OverwriteCurrentState {
  type: 'overwriteCurrent';
  inviter_name: string;
  inviter_uid: string;
  partner_name: string;
  partner_email: string;
}

export type PartnerState =
  | UnivitedState
  | WaitingState
  | ConfirmState
  | AcceptedState
  | OverwriteState
  | OverwriteCurrentState;

export type InviteState = 'uninvited' | 'confirm' | 'waiting' | 'accepted';
export type InviteMediums = 'email' | 'phone' | 'link';

export type InviteType = 'Normal' | 'Forced';

export type NameError = 'None' | 'Empty' | 'IsEmail';
export type EmailError = 'None' | 'Empty' | 'SelfInvite';
export interface PartnerEmailErrors {
  nameError: NameError;
  emailError: EmailError;
  alreadyInGroup: boolean;
}

export type Return = [
  boolean,
  (firstName: string, email: string) => void,
  PartnerEmailErrors,
  QueryResult<PartnerState>
];
