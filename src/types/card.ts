export interface Card {
  card_number: string;
  account_id: number;
}

export interface CreateCardRequest {
  card_number: string;
  account_id: number;
}

export interface UpdateCardRequest {
  account_id?: number;
}
