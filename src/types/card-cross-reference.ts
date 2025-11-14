export interface CardCrossReference {
  accountId: number;
  customerId: number;
  createdAt?: string;
  updatedAt?: string;
}

export interface CreateCardCrossReferenceRequest {
  accountId: number;
  customerId: number;
}

export interface UpdateCardCrossReferenceRequest {
  customerId?: number;
}
