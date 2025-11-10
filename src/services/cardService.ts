import { Card, CardListResponse, CardListFilters, CreateCardRequest, UpdateCardRequest } from '@/types/card';

const API_BASE_URL = '/api';

class CardService {
  private getAuthHeaders(): Record<string, string> {
    const token = localStorage.getItem('access_token');
    return {
      'Content-Type': 'application/json',
      ...(token && { Authorization: `Bearer ${token}` }),
    };
  }

  async getCardsList(filters: CardListFilters = {}): Promise<CardListResponse> {
    const params = new URLSearchParams();
    
    if (filters.accountId) params.append('accountId', filters.accountId);
    if (filters.cardNumber) params.append('cardNumber', filters.cardNumber);
    if (filters.page !== undefined) params.append('page', filters.page.toString());
    if (filters.size !== undefined) params.append('size', filters.size.toString());
    if (filters.sort) params.append('sort', filters.sort);

    const queryString = params.toString();
    const url = `${API_BASE_URL}/credit-cards/list${queryString ? `?${queryString}` : ''}`;

    const response = await fetch(url, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    
    if (!response.ok) {
      throw new Error('Failed to fetch cards list');
    }
    
    return response.json();
  }

  async getCardByNumber(cardNumber: string): Promise<Card> {
    const response = await fetch(`${API_BASE_URL}/credit-cards/${cardNumber}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    
    if (!response.ok) {
      throw new Error('Failed to fetch card');
    }
    
    return response.json();
  }

  async getCardsByAccount(accountId: string): Promise<Card[]> {
    const response = await fetch(`${API_BASE_URL}/credit-cards/account/${accountId}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    
    if (!response.ok) {
      throw new Error('Failed to fetch cards by account');
    }
    
    return response.json();
  }

  async createCard(data: CreateCardRequest): Promise<Card> {
    const response = await fetch(`${API_BASE_URL}/credit-cards`, {
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
    const response = await fetch(`${API_BASE_URL}/credit-cards/${cardNumber}`, {
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
    const response = await fetch(`${API_BASE_URL}/credit-cards/${cardNumber}`, {
      method: 'DELETE',
      headers: this.getAuthHeaders(),
    });
    
    if (!response.ok) {
      throw new Error('Failed to delete card');
    }
  }
}

export const cardService = new CardService();
