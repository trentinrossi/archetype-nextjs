import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

// GET /api/credit-cards - List all credit cards (Admin access)
export async function GET(request: NextRequest) {
  try {
    const { searchParams } = new URL(request.url);
    const page = searchParams.get('page') || '0';
    const size = searchParams.get('size') || '20';
    const sort = searchParams.get('sort') || '';
    
    let queryString = `/api/credit-cards?page=${page}&size=${size}`;
    if (sort) {
      queryString += `&sort=${sort}`;
    }
    
    const response = await forwardAuthRequest(queryString, 'GET', request);
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error fetching credit cards:', error);
    return NextResponse.json({ error: 'Failed to fetch credit cards' }, { status: 500 });
  }
}

// POST /api/credit-cards - Create new credit card
export async function POST(request: NextRequest) {
  try {
    const body = await request.json();
    
    // Validate card number (16 digits)
    if (!body.cardNumber || !/^\d{16}$/.test(body.cardNumber)) {
      return NextResponse.json(
        { error: 'Card number must be exactly 16 digits' },
        { status: 400 }
      );
    }
    
    // Validate account ID (11 digits)
    if (!body.accountId || !/^\d{11}$/.test(body.accountId)) {
      return NextResponse.json(
        { error: 'Account ID must be exactly 11 digits' },
        { status: 400 }
      );
    }
    
    // Validate card status
    if (!body.cardStatus || !/^[A-Z]$/.test(body.cardStatus)) {
      return NextResponse.json(
        { error: 'Card status must be a single uppercase letter' },
        { status: 400 }
      );
    }
    
    // Validate expiry month if provided
    if (body.expiryMonth && !/^(0[1-9]|1[0-2])$/.test(body.expiryMonth)) {
      return NextResponse.json(
        { error: 'Expiry month must be between 01 and 12' },
        { status: 400 }
      );
    }
    
    // Validate expiry year if provided
    if (body.expiryYear && !/^\d{4}$/.test(body.expiryYear)) {
      return NextResponse.json(
        { error: 'Expiry year must be 4 digits' },
        { status: 400 }
      );
    }
    
    const response = await forwardAuthRequest('/api/credit-cards', 'POST', request, body);
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error creating credit card:', error);
    return NextResponse.json({ error: 'Failed to create credit card' }, { status: 500 });
  }
}
