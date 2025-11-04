'use client';

import React, { useEffect, useState } from 'react';
import { useParams, useRouter } from 'next/navigation';
import { Button, Input } from '@/components/ui';
import { cardService } from '@/services/cardService';
import { UpdateCardData } from '@/types/account';

const EditCardPage: React.FC = () => {
  const params = useParams();
  const router = useRouter();
  const cardNumber = params.id as string;
  
  const [loading, setLoading] = useState(true);
  const [submitting, setSubmitting] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const [formData, setFormData] = useState<UpdateCardData>({
    cvvCode: '',
    embossedName: '',
    expirationDate: '',
    activeStatus: 'Y',
  });

  const [fieldErrors, setFieldErrors] = useState<Record<string, string>>({});
  const [originalCardNumber, setOriginalCardNumber] = useState('');

  useEffect(() => {
    const fetchCard = async () => {
      try {
        setLoading(true);
        setError(null);
        const data = await cardService.getCardByCardNumber(cardNumber);
        
        // Store original card number for display
        setOriginalCardNumber(data.cardNumber);
        
        // Populate form with existing data
        setFormData({
          cvvCode: data.cvvCode,
          embossedName: data.embossedName,
          expirationDate: data.expirationDate.split('T')[0],
          activeStatus: data.activeStatus,
        });
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

  const handleInputChange = (e: React.ChangeEvent<HTMLInputElement | HTMLSelectElement>) => {
    const { name, value } = e.target;
    
    // Clear field error when user starts typing
    if (fieldErrors[name]) {
      setFieldErrors(prev => {
        const newErrors = { ...prev };
        delete newErrors[name];
        return newErrors;
      });
    }

    // Convert embossed name to uppercase
    if (name === 'embossedName') {
      setFormData(prev => ({
        ...prev,
        [name]: value.toUpperCase(),
      }));
    } else {
      setFormData(prev => ({
        ...prev,
        [name]: value,
      }));
    }
  };

  const validateForm = (): boolean => {
    const errors: Record<string, string> = {};

    if (!formData.embossedName?.trim()) {
      errors.embossedName = 'Embossed name is required';
    }

    if (!formData.cvvCode?.trim()) {
      errors.cvvCode = 'CVV code is required';
    } else if (!/^\d{3,4}$/.test(formData.cvvCode)) {
      errors.cvvCode = 'CVV must be 3 or 4 digits';
    }

    if (!formData.expirationDate) {
      errors.expirationDate = 'Expiration date is required';
    } else {
      const expDate = new Date(formData.expirationDate);
      const today = new Date();
      today.setHours(0, 0, 0, 0);
      
      if (expDate <= today) {
        errors.expirationDate = 'Expiration date must be in the future';
      }
    }

    setFieldErrors(errors);
    return Object.keys(errors).length === 0;
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    
    if (!validateForm()) {
      setError('Please fix the errors in the form');
      return;
    }

    try {
      setSubmitting(true);
      setError(null);

      await cardService.updateCard(cardNumber, formData);
      
      // Redirect to card details on success
      router.push(`/cards/${cardNumber}`);
    } catch (err) {
      console.error('Error updating card:', err);
      setError(err instanceof Error ? err.message : 'Failed to update card');
      setSubmitting(false);
    }
  };

  const handleCancel = () => {
    router.push(`/cards/${cardNumber}`);
  };

  const formatCardNumberDisplay = (cardNum: string): string => {
    if (!cardNum) return '';
    const cleaned = cardNum.replace(/\s/g, '');
    return cleaned.match(/.{1,4}/g)?.join(' ') || cardNum;
  };

  if (loading) {
    return (
      <div className="min-h-screen flex items-center justify-center">
        <div className="text-center">
          <div className="inline-block animate-spin rounded-full h-12 w-12 border-b-2 border-gray-900"></div>
          <p className="mt-4 text-gray-600">Loading card...</p>
        </div>
      </div>
    );
  }

  return (
    <div className="container mx-auto px-4 py-8">
      <div className="mb-8">
        <h1 className="text-3xl font-bold text-gray-900 mb-2">Edit Card</h1>
        <p className="text-gray-600 font-mono">Card Number: {formatCardNumberDisplay(originalCardNumber)}</p>
      </div>

      <div className="bg-white shadow-md rounded-lg overflow-hidden">
        <form onSubmit={handleSubmit} className="p-6">
          {error && (
            <div className="mb-6 bg-red-50 border border-red-200 text-red-800 px-4 py-3 rounded-md">
              <div className="flex">
                <div className="flex-shrink-0">
                  <svg className="h-5 w-5 text-red-400" viewBox="0 0 20 20" fill="currentColor">
                    <path fillRule="evenodd" d="M10 18a8 8 0 100-16 8 8 0 000 16zM8.707 7.293a1 1 0 00-1.414 1.414L8.586 10l-1.293 1.293a1 1 0 101.414 1.414L10 11.414l1.293 1.293a1 1 0 001.414-1.414L11.414 10l1.293-1.293a1 1 0 00-1.414-1.414L10 8.586 8.707 7.293z" clipRule="evenodd" />
                  </svg>
                </div>
                <div className="ml-3">
                  <p className="text-sm font-medium">{error}</p>
                </div>
              </div>
            </div>
          )}

          {/* Card Details Section */}
          <div className="mb-8">
            <h2 className="text-xl font-semibold text-gray-900 mb-4 pb-2 border-b border-gray-200">
              Card Details
            </h2>
            <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
              <div>
                <label htmlFor="embossedName" className="block text-sm font-medium text-gray-700 mb-1">
                  Embossed Name <span className="text-red-500">*</span>
                </label>
                <Input
                  id="embossedName"
                  name="embossedName"
                  type="text"
                  value={formData.embossedName}
                  onChange={handleInputChange}
                  placeholder="JOHN DOE"
                  className={fieldErrors.embossedName ? 'border-red-500' : ''}
                  disabled={submitting}
                />
                {fieldErrors.embossedName && (
                  <p className="mt-1 text-sm text-red-600">{fieldErrors.embossedName}</p>
                )}
                <p className="mt-1 text-xs text-gray-500">Name as it appears on the card (automatically capitalized)</p>
              </div>

              <div>
                <label htmlFor="cvvCode" className="block text-sm font-medium text-gray-700 mb-1">
                  CVV Code <span className="text-red-500">*</span>
                </label>
                <Input
                  id="cvvCode"
                  name="cvvCode"
                  type="password"
                  value={formData.cvvCode}
                  onChange={handleInputChange}
                  placeholder="123"
                  maxLength={4}
                  className={fieldErrors.cvvCode ? 'border-red-500' : ''}
                  disabled={submitting}
                />
                {fieldErrors.cvvCode && (
                  <p className="mt-1 text-sm text-red-600">{fieldErrors.cvvCode}</p>
                )}
                <p className="mt-1 text-xs text-gray-500">3 or 4 digit security code</p>
              </div>

              <div>
                <label htmlFor="expirationDate" className="block text-sm font-medium text-gray-700 mb-1">
                  Expiration Date <span className="text-red-500">*</span>
                </label>
                <Input
                  id="expirationDate"
                  name="expirationDate"
                  type="date"
                  value={formData.expirationDate}
                  onChange={handleInputChange}
                  className={fieldErrors.expirationDate ? 'border-red-500' : ''}
                  disabled={submitting}
                />
                {fieldErrors.expirationDate && (
                  <p className="mt-1 text-sm text-red-600">{fieldErrors.expirationDate}</p>
                )}
                <p className="mt-1 text-xs text-gray-500">Card expiration date (must be in the future)</p>
              </div>

              <div>
                <label htmlFor="activeStatus" className="block text-sm font-medium text-gray-700 mb-1">
                  Card Status <span className="text-red-500">*</span>
                </label>
                <select
                  id="activeStatus"
                  name="activeStatus"
                  value={formData.activeStatus}
                  onChange={handleInputChange}
                  className="flex h-10 w-full rounded-md border border-gray-300 bg-white px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-transparent disabled:cursor-not-allowed disabled:opacity-50"
                  disabled={submitting}
                >
                  <option value="Y">Active</option>
                  <option value="N">Inactive</option>
                </select>
                <p className="mt-1 text-xs text-gray-500">Only active cards can be used for transactions</p>
              </div>
            </div>
          </div>

          {/* Security Warning */}
          <div className="mt-6 bg-yellow-50 border border-yellow-200 rounded-md p-4">
            <div className="flex">
              <div className="flex-shrink-0">
                <svg className="h-5 w-5 text-yellow-400" viewBox="0 0 20 20" fill="currentColor">
                  <path fillRule="evenodd" d="M8.257 3.099c.765-1.36 2.722-1.36 3.486 0l5.58 9.92c.75 1.334-.213 2.98-1.742 2.98H4.42c-1.53 0-2.493-1.646-1.743-2.98l5.58-9.92zM11 13a1 1 0 11-2 0 1 1 0 012 0zm-1-8a1 1 0 00-1 1v3a1 1 0 002 0V6a1 1 0 00-1-1z" clipRule="evenodd" />
                </svg>
              </div>
              <div className="ml-3">
                <h3 className="text-sm font-medium text-yellow-800">Security Notice</h3>
                <div className="mt-2 text-sm text-yellow-700">
                  <ul className="list-disc list-inside space-y-1">
                    <li>Updating the CVV code will invalidate the previous code</li>
                    <li>Setting the card to inactive will prevent all transactions</li>
                    <li>Expired cards cannot be used regardless of active status</li>
                    <li>All changes are logged for security and compliance purposes</li>
                  </ul>
                </div>
              </div>
            </div>
          </div>

          {/* Information Box */}
          <div className="mt-6 bg-blue-50 border border-blue-200 rounded-md p-4">
            <div className="flex">
              <div className="flex-shrink-0">
                <svg className="h-5 w-5 text-blue-400" viewBox="0 0 20 20" fill="currentColor">
                  <path fillRule="evenodd" d="M18 10a8 8 0 11-16 0 8 8 0 0116 0zm-7-4a1 1 0 11-2 0 1 1 0 012 0zM9 9a1 1 0 000 2v3a1 1 0 001 1h1a1 1 0 100-2v-3a1 1 0 00-1-1H9z" clipRule="evenodd" />
                </svg>
              </div>
              <div className="ml-3">
                <h3 className="text-sm font-medium text-blue-800">Update Information</h3>
                <div className="mt-2 text-sm text-blue-700">
                  <p>Note: The card number and associated account ID cannot be changed after card creation. To change these details, you must issue a new card.</p>
                </div>
              </div>
            </div>
          </div>

          {/* Form Actions */}
          <div className="mt-8 flex justify-end gap-3 pt-6 border-t border-gray-200">
            <Button
              type="button"
              variant="secondary"
              onClick={handleCancel}
              disabled={submitting}
            >
              Cancel
            </Button>
            <Button
              type="submit"
              disabled={submitting}
            >
              {submitting ? 'Updating...' : 'Update Card'}
            </Button>
          </div>
        </form>
      </div>
    </div>
  );
};

export default EditCardPage;
