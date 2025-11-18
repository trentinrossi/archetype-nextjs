import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

export async function GET(request: NextRequest) {
  try {
    const { searchParams } = new URL(request.url);
    const queryString = searchParams.toString();
    const endpoint = queryString ? `/api/credit-cards?${queryString}` : '/api/credit-cards';
    
    const response = await forwardAuthRequest(endpoint, 'GET', request);
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error fetching credit cards:', error);
    return NextResponse.json({ error: 'Failed to fetch credit cards' }, { status: 500 });
  }
}

export async function POST(request: NextRequest) {
  try {
    const body = await request.json();
    const response = await forwardAuthRequest('/api/credit-cards', 'POST', request, body);
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error creating credit card:', error);
    return NextResponse.json({ error: 'Failed to create credit card' }, { status: 500 });
  }
}
