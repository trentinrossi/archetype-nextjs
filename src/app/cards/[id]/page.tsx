'use client';

import React, { useEffect, useState } from 'react';
import { useParams, useRouter } from 'next/navigation';
import { Button } from '@/components/ui';
import { cardService } from '@/services/cardService';
import { Card } from '@/types/account';

const CardDetailPage: React.FC = () => {
  const params = useParams();
  const router = useRouter();
  const cardNumber = params.id as string;
  
  const [card, setCard] = useState<Card | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    const fetchCard = async () => {
      try {
        setLoading(true);
        setError(null);
        const data = await cardService.getCardByCardNumber(cardNumber);
        setCard(data);
      } catch (err) {
        console.error('Error fetching card:', err);
        setError(err instanceof Error ? err.message : 'Failed to fetch card');
      } finally {
        setLoading(false);
      }
    };

    if (cardNumber) {
      fetchCard();
    }
  }, [cardNumber]);

  const formatCardNumber = (cardNum: string): string => {
    if (!cardNum) return 'N/A';
    const cleaned = cardNum.replace(/\s/g, '');
    return cleaned.match(/.{1,4}/g)?.join(' ') || cardNum;
  };

  const maskCardNumber = (cardNum: string): string => {
    if (!cardNum) return 'N/A';
    const cleaned = cardNum.replace(/\s/g, '');
    if (cleaned.length >= 4) {
      return `•••• •••• •••• ${cleaned.slice(-4)}`;
    }
    return cardNum;
  };

  const formatExpirationDate = (dateString: string): string => {
    if (!dateString) return 'N/A';
    const date = new Date(dateString);
    const month = String(date.getMonth() + 1).padStart(2, '0');
    const year = String(date.getFullYear()).slice(-2);
    return `${month}/${year}`;
  };

  const isCardExpired = (dateString: string): boolean => {
    if (!dateString) return false;
    const expDate = new Date(dateString);
    const today = new Date();
    return expDate < today;
  };

  const getExpirationStatus = (dateString: string): { text: string; color: string } => {
    if (!dateString) return { text: 'Unknown', color: 'gray' };
    
    const expDate = new Date(dateString);
    const today = new Date();
    const diffMonths = (expDate.getFullYear() - today.getFullYear()) * 12 + 
                       (expDate.getMonth() - today.getMonth());

    if (expDate < today) {
      return { text: 'Expired', color: 'red' };
    } else if (diffMonths <= 3) {
      return { text: 'Expiring Soon', color: 'yellow' };
    } else {
      return { text: 'Valid', color: 'green' };
    }
  };

  if (loading) {
    return (
      <div className="min-h-screen flex items-center justify-center">
        <div className="text-center">
          <div className="inline-block animate-spin rounded-full h-12 w-12 border-b-2 border-gray-900"></div>
          <p className="mt-4 text-gray-600">Loading card details...</p>
        </div>
      </div>
    );
  }

  if (error || !card) {
    return (
      <div className="min-h-screen flex items-center justify-center">
        <div className="text-center">
          <div className="text-red-600 text-xl mb-4">Error</div>
          <p className="text-gray-600 mb-4">{error || 'Card not found'}</p>
          <Button onClick={() => router.push('/cards')}>Back to Cards</Button>
        </div>
      </div>
    );
  }

  const expirationStatus = getExpirationStatus(card.expirationDate);

  return (
    <div className="container mx-auto px-4 py-8">
      {/* Header */}
      <div className="mb-8">
        <div className="flex items-center justify-between">
          <div>
            <h1 className="text-3xl font-bold text-gray-900 mb-2">Card Details</h1>
            <p className="text-gray-600 font-mono">{maskCardNumber(card.cardNumber)}</p>
          </div>
          <div className="flex gap-3">
            <Button variant="secondary" onClick={() => router.push('/cards')}>
              Back to List
            </Button>
            <Button onClick={() => router.push(`/cards/${cardNumber}/edit`)}>
              Edit Card
            </Button>
          </div>
        </div>
      </div>

      <div className="grid grid-cols-1 lg:grid-cols-3 gap-6">
        {/* Main Content */}
        <div className="lg:col-span-2 space-y-6">
          {/* Visual Card Display */}
          <div className="bg-gradient-to-br from-gray-800 via-gray-900 to-black text-white shadow-2xl rounded-2xl p-8 relative overflow-hidden">
            {/* Card Background Pattern */}
            <div className="absolute top-0 right-0 w-64 h-64 bg-gradient-to-br from-blue-500/20 to-purple-500/20 rounded-full blur-3xl -mr-32 -mt-32" />
            <div className="absolute bottom-0 left-0 w-48 h-48 bg-gradient-to-tr from-green-500/20 to-blue-500/20 rounded-full blur-3xl -ml-24 -mb-24" />
            
            <div className="relative z-10">
              {/* Card Chip */}
              <div className="mb-8">
                <div className="w-12 h-10 bg-gradient-to-br from-yellow-200 to-yellow-400 rounded-md"></div>
              </div>

              {/* Card Number */}
              <div className="mb-8">
                <div className="text-sm text-gray-400 mb-2">Card Number</div>
                <div className="text-2xl font-mono tracking-wider">{formatCardNumber(card.cardNumber)}</div>
              </div>

              <div className="flex justify-between items-end">
                {/* Cardholder Name */}
                <div>
                  <div className="text-xs text-gray-400 mb-1">Cardholder Name</div>
                  <div className="text-lg font-semibold uppercase tracking-wide">{card.embossedName}</div>
                </div>

                {/* Expiration Date */}
                <div className="text-right">
                  <div className="text-xs text-gray-400 mb-1">Expires</div>
                  <div className="text-lg font-mono">{formatExpirationDate(card.expirationDate)}</div>
                </div>
              </div>
            </div>
          </div>

          {/* Card Information */}
          <div className="bg-white shadow-md rounded-lg overflow-hidden">
            <div className="px-6 py-4 bg-gray-50 border-b border-gray-200">
              <h2 className="text-xl font-semibold text-gray-900">Card Information</h2>
            </div>
            <div className="p-6 grid grid-cols-1 md:grid-cols-2 gap-6">
              <div>
                <label className="block text-sm font-medium text-gray-500 mb-1">Card Number</label>
                <p className="text-base text-gray-900 font-mono">{formatCardNumber(card.cardNumber)}</p>
              </div>
              <div>
                <label className="block text-sm font-medium text-gray-500 mb-1">Account ID</label>
                <p className="text-base text-gray-900">
                  <button
                    onClick={() => router.push(`/accounts/${card.accountId}`)}
                    className="text-blue-600 hover:text-blue-800 hover:underline"
                  >
                    {card.accountId}
                  </button>
                </p>
              </div>
              <div>
                <label className="block text-sm font-medium text-gray-500 mb-1">Embossed Name</label>
                <p className="text-base text-gray-900 uppercase">{card.embossedName}</p>
              </div>
              <div>
                <label className="block text-sm font-medium text-gray-500 mb-1">CVV Code</label>
                <p className="text-base text-gray-900 font-mono">•••</p>
                <p className="text-xs text-gray-500 mt-1">Hidden for security</p>
              </div>
              <div>
                <label className="block text-sm font-medium text-gray-500 mb-1">Expiration Date</label>
                <p className="text-base text-gray-900">{formatExpirationDate(card.expirationDate)}</p>
              </div>
              <div>
                <label className="block text-sm font-medium text-gray-500 mb-1">Card Status</label>
                <div className="mt-1">
                  {card.activeStatus === 'Y' ? (
                    <span className="inline-flex items-center px-3 py-1 rounded-full text-sm font-medium bg-green-100 text-green-800">
                      Active
                    </span>
                  ) : (
                    <span className="inline-flex items-center px-3 py-1 rounded-full text-sm font-medium bg-red-100 text-red-800">
                      Inactive
                    </span>
                  )}
                </div>
              </div>
            </div>
          </div>

          {/* Security Notice */}
          <div className="bg-yellow-50 border border-yellow-200 rounded-lg p-4">
            <div className="flex">
              <div className="flex-shrink-0">
                <svg className="h-5 w-5 text-yellow-400" viewBox="0 0 20 20" fill="currentColor">
                  <path fillRule="evenodd" d="M8.257 3.099c.765-1.36 2.722-1.36 3.486 0l5.58 9.92c.75 1.334-.213 2.98-1.742 2.98H4.42c-1.53 0-2.493-1.646-1.743-2.98l5.58-9.92zM11 13a1 1 0 11-2 0 1 1 0 012 0zm-1-8a1 1 0 00-1 1v3a1 1 0 002 0V6a1 1 0 00-1-1z" clipRule="evenodd" />
                </svg>
              </div>
              <div className="ml-3">
                <h3 className="text-sm font-medium text-yellow-800">Security Notice</h3>
                <div className="mt-2 text-sm text-yellow-700">
                  <p>The CVV code is hidden for security purposes. Full card details should only be accessed by authorized personnel.</p>
                </div>
              </div>
            </div>
          </div>
        </div>

        {/* Sidebar */}
        <div className="lg:col-span-1 space-y-6">
          {/* Status Card */}
          <div className="bg-white shadow-md rounded-lg overflow-hidden">
            <div className="px-6 py-4 bg-gray-50 border-b border-gray-200">
              <h2 className="text-xl font-semibold text-gray-900">Card Status</h2>
            </div>
            <div className="p-6">
              <div className="text-center mb-4">
                <div className={`inline-flex items-center px-4 py-2 rounded-full text-lg font-semibold ${
                  expirationStatus.color === 'green' ? 'bg-green-100 text-green-800' :
                  expirationStatus.color === 'yellow' ? 'bg-yellow-100 text-yellow-800' :
                  'bg-red-100 text-red-800'
                }`}>
                  {expirationStatus.text}
                </div>
              </div>
              
              <div className="space-y-3 text-sm">
                <div className="flex justify-between">
                  <span className="text-gray-600">Activation Status:</span>
                  <span className="font-medium text-gray-900">
                    {card.activeStatus === 'Y' ? 'Active' : 'Inactive'}
                  </span>
                </div>
                <div className="flex justify-between">
                  <span className="text-gray-600">Expires:</span>
                  <span className="font-medium text-gray-900">{formatExpirationDate(card.expirationDate)}</span>
                </div>
                {isCardExpired(card.expirationDate) && (
                  <div className="mt-4 p-3 bg-red-50 border border-red-200 rounded-md">
                    <p className="text-xs text-red-800">This card has expired and cannot be used for transactions.</p>
                  </div>
                )}
              </div>
            </div>
          </div>

          {/* Quick Actions */}
          <div className="bg-blue-50 border border-blue-200 rounded-lg p-4">
            <h3 className="text-sm font-medium text-blue-900 mb-3">Quick Actions</h3>
            <div className="space-y-2">
              <Button
                variant="secondary"
                size="sm"
                className="w-full justify-start"
                onClick={() => router.push(`/accounts/${card.accountId}`)}
              >
                View Account
              </Button>
              <Button
                variant="secondary"
                size="sm"
                className="w-full justify-start"
                onClick={() => router.push(`/transactions?card=${cardNumber}`)}
              >
                View Transactions
              </Button>
              <Button
                variant="secondary"
                size="sm"
                className="w-full justify-start"
                onClick={() => router.push(`/statements?card=${cardNumber}`)}
              >
                View Statements
              </Button>
            </div>
          </div>

          {/* Card Type Info */}
          <div className="bg-white shadow-md rounded-lg overflow-hidden">
            <div className="px-6 py-4 bg-gray-50 border-b border-gray-200">
              <h2 className="text-xl font-semibold text-gray-900">Card Details</h2>
            </div>
            <div className="p-6 space-y-3 text-sm">
              <div className="flex justify-between">
                <span className="text-gray-600">Card Type:</span>
                <span className="font-medium text-gray-900">Credit Card</span>
              </div>
              <div className="flex justify-between">
                <span className="text-gray-600">Network:</span>
                <span className="font-medium text-gray-900">Visa</span>
              </div>
              <div className="flex justify-between">
                <span className="text-gray-600">Card Level:</span>
                <span className="font-medium text-gray-900">Standard</span>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};

export default CardDetailPage;
