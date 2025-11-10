/**
 * Card Type Definitions
 * Based on business rules for Credit Card List (CCRDLIA)
 */

export interface Card {
  cardNumber: string;
  accountId: string;
  embossedName: string;
  expirationDate: string;
  activeStatus: string;
  active: boolean;
  expired: boolean;
}

export interface CardListItem {
  cardNumber: string;
  accountId: string;
  cardStatus: string;
}

export interface CardListResponse {
  content: CardListItem[];
  totalElements: number;
  totalPages: number;
  size: number;
  number: number;
  first: boolean;
  last: boolean;
}

export interface CardListFilters {
  accountId?: string;
  cardNumber?: string;
  page?: number;
  size?: number;
}

export interface CreateCardRequest {
  cardNumber: string;
  accountId: string;
  embossedName: string;
  expirationDate: string;
  activeStatus: string;
}

export interface UpdateCardRequest {
  embossedName?: string;
  expirationDate?: string;
  activeStatus?: string;
}
