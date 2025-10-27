import { NextRequest } from 'next/server';

const API_BASE_URL = process.env.API_BASE_URL || 'http://localhost:8080';

export function getAuthHeaders(request: NextRequest): Record<string, string> {
  const authorization = request.headers.get('authorization');
  
  return {
    'Content-Type': 'application/json',
    ...(authorization && { Authorization: authorization }),
  };
}

export async function forwardAuthRequest(
  endpoint: string,
  method: string,
  request: NextRequest,
  body?: unknown
): Promise<Response> {
  const headers = getAuthHeaders(request);
  
  const fetchOptions: RequestInit = {
    method,
    headers,
  };
  
  if (body) {
    fetchOptions.body = JSON.stringify(body);
  }
  
  const response = await fetch(`${API_BASE_URL}${endpoint}`, fetchOptions);
  
  return response;
}

export async function handleAuthApiResponse(response: Response) {
  try {
    const data = await response.json();
    return { data, status: response.status, ok: response.ok };
  } catch (error) {
    console.error('Failed to parse API response:', error);
    return {
      data: { error: 'Failed to parse response' },
      status: 500,
      ok: false
    };
  }
}