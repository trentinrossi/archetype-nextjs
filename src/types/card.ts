export interface Card {
  cardNumber: string;
  accountId: string;
  cvvCode: string;
  embossedName: string;
  expirationDate: string;
  activeStatus: string;
  createdAt?: string;
  updatedAt?: string;
}

export interface CreateCardRequest {
  cardNumber: string;
  accountId: string;
  cvvCode: string;
  embossedName: string;
  expirationDate: string;
  activeStatus: string;
}

export interface UpdateCardRequest {
  accountId?: string;
  cvvCode?: string;
  embossedName?: string;
  expirationDate?: string;
  activeStatus?: string;
}
