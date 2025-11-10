/**
 * Card Type Definitions
 * Based on business rules for Credit Card List (CCRDLIA)
 */

export interface Card {
  cardNumber: string;          // 16-digit card number
  accountId: string;            // 11-digit account identifier
  embossedName: string;         // Name on card
  expirationDate: string;       // Card expiration date
  activeStatus: string;         // Y/N status
  active: boolean;              // Derived active status
  expired: boolean;             // Derived expiration status
}

export interface CardListDTO {
  cardNumber: string;
  accountId: string;
  activeStatus: string;
  embossedName?: string;
  expirationDate?: string;
}

export interface CardListResponse {
  content: CardListDTO[];
  totalElements: number;
  totalPages: number;
  size: number;
  number: number;
  first: boolean;
  last: boolean;
  empty: boolean;
}

export interface CardFilterCriteria {
  accountId?: string;
  cardNumber?: string;
  page?: number;
  size?: number;
  sort?: string;
}

export interface CardCreateRequest {
  cardNumber: string;
  accountId: string;
  embossedName: string;
  expirationDate: string;
  activeStatus: string;
  cvvCode: string;
}

export interface CardUpdateRequest {
  embossedName?: string;
  expirationDate?: string;
  activeStatus?: string;
}

export type SelectionCode = 'S' | 'U' | '';

export interface CardListItem extends CardListDTO {
  selectionCode: SelectionCode;
}
