export interface CardCrossReference {
  cardNumber: string;
  accountId: number;
  customerId: number;
  createdAt: string;
  updatedAt: string;
}

export interface CreateCardCrossReferenceRequest {
  cardNumber: string;
  accountId: number;
  customerId: number;
}

export interface ApiError {
  error: string;
  status: number;
  details?: string;
}
