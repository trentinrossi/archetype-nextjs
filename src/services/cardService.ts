import { 
  Card, 
  CreateCardRequest, 
  UpdateCardRequest 
} from '@/types/cardDemo';

const API_BASE_URL = '/api';

class CardService {
  private getAuthHeaders(): Record<string, string> {
    const token = localStorage.getItem('access_token');
    return {
      'Content-Type': 'application/json',
      ...(token && { Authorization: `Bearer ${token}` }),
    };
  }

  async getCards(): Promise<Card[]> {
    const response = await fetch(`${API_BASE_URL}/cards`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch cards');
    }
    return response.json();
  }

  async getCardByCardNumber(cardNumber: string): Promise<Card> {
    const response = await fetch(`${API_BASE_URL}/cards/${cardNumber}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch card');
    }
    return response.json();
  }

  async getCardsByAccountId(accountId: string): Promise<Card[]> {
    const response = await fetch(`${API_BASE_URL}/cards/account/${accountId}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch cards by account');
    }
    return response.json();
  }

  async getCardsByStatus(activeStatus: string): Promise<Card[]> {
    const response = await fetch(`${API_BASE_URL}/cards/status/${activeStatus}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch cards by status');
    }
    return response.json();
  }

  async createCard(data: CreateCardRequest): Promise<Card> {
    const response = await fetch(`${API_BASE_URL}/cards`, {
      method: 'POST',
      headers: this.getAuthHeaders(),
      body: JSON.stringify(data),
    });
    if (!response.ok) {
      throw new Error('Failed to create card');
    }
    return response.json();
  }

  async updateCard(cardNumber: string, data: UpdateCardRequest): Promise<Card> {
    const response = await fetch(`${API_BASE_URL}/cards/${cardNumber}`, {
      method: 'PUT',
      headers: this.getAuthHeaders(),
      body: JSON.stringify(data),
    });
    if (!response.ok) {
      throw new Error('Failed to update card');
    }
    return response.json();
  }

  async deleteCard(cardNumber: string): Promise<void> {
    const response = await fetch(`${API_BASE_URL}/cards/${cardNumber}`, {
      method: 'DELETE',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to delete card');
    }
  }
}

export const cardService = new CardService();
