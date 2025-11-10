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
  activeStatus: string;
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
  sort?: string;
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
