export interface Card {
  cardNumber: string;
  status: string;
  cardDetails?: string;
  createdAt: string;
  updatedAt: string;
}

export interface CreateCardRequest {
  cardNumber: string;
  status: string;
  cardDetails?: string;
}

export interface PaginatedCardsResponse {
  content: Card[];
  totalElements: number;
  totalPages: number;
  size: number;
  number: number;
}

export interface ApiError {
  error: string;
  status: number;
  details?: string;
}
