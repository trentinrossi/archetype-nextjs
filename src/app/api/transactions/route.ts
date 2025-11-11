import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

// GET /api/transactions - List all transactions with pagination
export async function GET(request: NextRequest) {
  try {
    const { searchParams } = new URL(request.url);
    const page = searchParams.get('page') || '0';
    const size = searchParams.get('size') || '20';
    const sort = searchParams.get('sort') || '';
    
    let queryString = `?page=${page}&size=${size}`;
    if (sort) {
      queryString += `&sort=${sort}`;
    }
    
    const response = await forwardAuthRequest(`/api/transactions${queryString}`, 'GET', request);
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error fetching transactions:', error);
    return NextResponse.json({ error: 'Failed to fetch transactions' }, { status: 500 });
  }
}

// POST /api/transactions - Create a new transaction
export async function POST(request: NextRequest) {
  try {
    const body = await request.json();
    const response = await forwardAuthRequest('/api/transactions', 'POST', request, body);
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error creating transaction:', error);
    return NextResponse.json({ error: 'Failed to create transaction' }, { status: 500 });
  }
}
