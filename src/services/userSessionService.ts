import { UserSession, CreateUserSessionRequest, UpdateUserSessionRequest } from '@/types/user-session';

const API_BASE_URL = '/api';

class UserSessionService {
  private getAuthHeaders(): Record<string, string> {
    const token = localStorage.getItem('access_token');
    return {
      'Content-Type': 'application/json',
      ...(token && { Authorization: `Bearer ${token}` }),
    };
  }

  async getUserSessions(page: number = 0, size: number = 20, sort?: string): Promise<any> {
    const queryParams = new URLSearchParams({
      page: page.toString(),
      size: size.toString(),
      ...(sort && { sort }),
    });
    
    const response = await fetch(`${API_BASE_URL}/user-sessions?${queryParams}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch user sessions');
    }
    return response.json();
  }

  async getUserSessionById(id: number): Promise<UserSession> {
    const response = await fetch(`${API_BASE_URL}/user-sessions/${id}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch user session');
    }
    return response.json();
  }

  async getUserSessionByTransactionId(transactionId: string): Promise<UserSession> {
    const response = await fetch(`${API_BASE_URL}/user-sessions/transaction/${transactionId}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch user session by transaction');
    }
    return response.json();
  }

  async getUserSessionsByProgramName(programName: string): Promise<UserSession[]> {
    const response = await fetch(`${API_BASE_URL}/user-sessions/program/${programName}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch user sessions by program');
    }
    return response.json();
  }

  async getUserSessionsByReenterFlag(reenterFlag: boolean): Promise<UserSession[]> {
    const response = await fetch(`${API_BASE_URL}/user-sessions/reenter/${reenterFlag}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch user sessions by reenter flag');
    }
    return response.json();
  }

  async getUserSessionsWithCallingContext(): Promise<UserSession[]> {
    const response = await fetch(`${API_BASE_URL}/user-sessions/with-calling-context`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch user sessions with calling context');
    }
    return response.json();
  }

  async getUserSessionsWithProgramContext(): Promise<UserSession[]> {
    const response = await fetch(`${API_BASE_URL}/user-sessions/with-program-context`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch user sessions with program context');
    }
    return response.json();
  }

  async getUserSessionsByFromProgram(fromProgram: string): Promise<UserSession[]> {
    const response = await fetch(`${API_BASE_URL}/user-sessions/from-program/${fromProgram}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch user sessions by from program');
    }
    return response.json();
  }

  async getUserSessionsByFromTransaction(fromTransaction: string): Promise<UserSession[]> {
    const response = await fetch(`${API_BASE_URL}/user-sessions/from-transaction/${fromTransaction}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch user sessions by from transaction');
    }
    return response.json();
  }

  async createUserSession(data: CreateUserSessionRequest): Promise<UserSession> {
    const response = await fetch(`${API_BASE_URL}/user-sessions`, {
      method: 'POST',
      headers: this.getAuthHeaders(),
      body: JSON.stringify(data),
    });
    if (!response.ok) {
      throw new Error('Failed to create user session');
    }
    return response.json();
  }

  async updateUserSession(id: number, data: UpdateUserSessionRequest): Promise<UserSession> {
    const response = await fetch(`${API_BASE_URL}/user-sessions/${id}`, {
      method: 'PUT',
      headers: this.getAuthHeaders(),
      body: JSON.stringify(data),
    });
    if (!response.ok) {
      throw new Error('Failed to update user session');
    }
    return response.json();
  }

  async deleteUserSession(id: number): Promise<void> {
    const response = await fetch(`${API_BASE_URL}/user-sessions/${id}`, {
      method: 'DELETE',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to delete user session');
    }
  }

  async clearCallingContext(id: number): Promise<UserSession> {
    const response = await fetch(`${API_BASE_URL}/user-sessions/${id}/clear-calling-context`, {
      method: 'POST',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to clear calling context');
    }
    return response.json();
  }

  async setReenterFlag(id: number, reenterFlag: boolean): Promise<UserSession> {
    const response = await fetch(`${API_BASE_URL}/user-sessions/${id}/set-reenter-flag?reenterFlag=${reenterFlag}`, {
      method: 'POST',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to set reenter flag');
    }
    return response.json();
  }
}

export const userSessionService = new UserSessionService();
