import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

// GET /api/credit-cards - List all credit cards
export async function GET(request: NextRequest) {
  try {
    const { searchParams } = new URL(request.url);
    const page = searchParams.get('page') || '0';
    const size = searchParams.get('size') || '20';
    const sort = searchParams.get('sort') || '';
    
    let url = `/api/credit-cards?page=${page}&size=${size}`;
    if (sort) {
      url += `&sort=${sort}`;
    }
    
    const response = await forwardAuthRequest(url, 'GET', request);
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error fetching credit cards:', error);
    return NextResponse.json(
      { error: 'Failed to fetch credit cards' },
      { status: 500 }
    );
  }
}

// POST /api/credit-cards - Create new credit card
export async function POST(request: NextRequest) {
  try {
    const body = await request.json();
    const response = await forwardAuthRequest(
      '/api/credit-cards',
      'POST',
      request,
      body
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error creating credit card:', error);
    return NextResponse.json(
      { error: 'Failed to create credit card' },
      { status: 500 }
    );
  }
}
