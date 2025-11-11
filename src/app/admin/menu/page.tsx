'use client';

import React, { useEffect, useState, useCallback } from 'react';
import { useRouter } from 'next/navigation';
import { adminMenuOptionService } from '@/services/adminMenuOptionService';
import { AdminMenuOption } from '@/types/admin-menu-option';
import { Button, Input } from '@/components/ui';

export default function AdminMenuPage() {
  const router = useRouter();
  const [menuOptions, setMenuOptions] = useState<AdminMenuOption[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [selectedOption, setSelectedOption] = useState<string>('');
  const [errorMessage, setErrorMessage] = useState<string>('');
  const [currentDate, setCurrentDate] = useState<string>('');
  const [currentTime, setCurrentTime] = useState<string>('');

  const fetchMenuOptions = useCallback(async () => {
    try {
      setLoading(true);
      const data = await adminMenuOptionService.getActiveAdminMenuOptions();
      setMenuOptions(data);
      setError(null);
    } catch (err) {
      setError('Failed to load admin menu options');
      console.error(err);
    } finally {
      setLoading(false);
    }
  }, []);

  useEffect(() => {
    fetchMenuOptions();
    
    const updateDateTime = () => {
      const now = new Date();
      setCurrentDate(now.toLocaleDateString('en-US', { 
        year: 'numeric', 
        month: '2-digit', 
        day: '2-digit' 
      }));
      setCurrentTime(now.toLocaleTimeString('en-US', { 
        hour: '2-digit', 
        minute: '2-digit', 
        second: '2-digit',
        hour12: false
      }));
    };
    
    updateDateTime();
    const interval = setInterval(updateDateTime, 1000);
    
    return () => clearInterval(interval);
  }, [fetchMenuOptions]);

  const handleSelectOption = (e: React.FormEvent) => {
    e.preventDefault();
    setErrorMessage('');

    if (!selectedOption.trim()) {
      setErrorMessage('Please enter a valid option number...');
      return;
    }

    const optionNum = parseInt(selectedOption, 10);
    
    if (isNaN(optionNum)) {
      setErrorMessage('Please enter a valid option number...');
      return;
    }

    const option = menuOptions.find(opt => opt.optionNumber === optionNum);
    
    if (!option) {
      setErrorMessage('Please enter a valid option number...');
      return;
    }

    if (!option.isActive) {
      setErrorMessage('This option is coming soon ...');
      return;
    }

    router.push(`/admin/function/${option.programName.toLowerCase()}`);
  };

  const handleExit = () => {
    router.push('/');
  };

  const handleKeyDown = (e: React.KeyboardEvent) => {
    if (e.key === 'F3') {
      e.preventDefault();
      handleExit();
    }
  };

  if (loading) {
    return (
      <div className="flex items-center justify-center min-h-screen bg-gray-50">
        <div className="text-lg">Loading...</div>
      </div>
    );
  }

  if (error) {
    return (
      <div className="flex items-center justify-center min-h-screen bg-gray-50">
        <div className="text-lg text-red-600">Error: {error}</div>
      </div>
    );
  }

  return (
    <div className="min-h-screen bg-gray-50 p-6" onKeyDown={handleKeyDown}>
      <div className="max-w-4xl mx-auto bg-white shadow-lg rounded-lg overflow-hidden">
        {/* Header Section */}
        <div className="bg-blue-600 text-white p-6">
          <div className="flex justify-between items-start mb-4">
            <div>
              <h1 className="text-3xl font-bold mb-2">CardDemo</h1>
              <h2 className="text-xl">Administration</h2>
            </div>
            <div className="text-right text-sm">
              <div className="mb-1">
                <span className="font-semibold">Transaction:</span> COADM1A
              </div>
              <div className="mb-1">
                <span className="font-semibold">Program:</span> COADM01C
              </div>
              <div className="mb-1">
                <span className="font-semibold">Date:</span> {currentDate}
              </div>
              <div>
                <span className="font-semibold">Time:</span> {currentTime}
              </div>
            </div>
          </div>
        </div>

        {/* Menu Options Section */}
        <div className="p-6">
          <h3 className="text-xl font-semibold mb-4 text-gray-800">
            Please select an option:
          </h3>
          
          <div className="space-y-2 mb-6">
            {menuOptions.length === 0 ? (
              <div className="text-center text-gray-500 py-8">
                No menu options available
              </div>
            ) : (
              menuOptions.map((option) => (
                <div 
                  key={option.id} 
                  className="flex items-center py-2 px-4 hover:bg-gray-50 rounded cursor-pointer"
                  onClick={() => {
                    setSelectedOption(option.optionNumber.toString());
                    setErrorMessage('');
                  }}
                >
                  <span className="font-mono text-lg mr-4 text-blue-600 font-semibold">
                    {option.optionNumber.toString().padStart(2, '0')}
                  </span>
                  <span className="text-gray-800">
                    {option.optionName}
                  </span>
                  {!option.isActive && (
                    <span className="ml-auto text-xs bg-yellow-100 text-yellow-800 px-2 py-1 rounded">
                      Coming Soon
                    </span>
                  )}
                </div>
              ))
            )}
          </div>

          {/* User Input Section */}
          <form onSubmit={handleSelectOption} className="space-y-4">
            <div className="flex items-end gap-4">
              <div className="flex-1">
                <Input
                  label="Enter Option Number"
                  type="text"
                  value={selectedOption}
                  onChange={(e) => {
                    setSelectedOption(e.target.value);
                    setErrorMessage('');
                  }}
                  placeholder="Enter option number"
                  autoFocus
                />
              </div>
              <Button type="submit" className="mb-0">
                Select
              </Button>
              <Button 
                type="button" 
                variant="secondary" 
                onClick={handleExit}
                className="mb-0"
              >
                Exit (F3)
              </Button>
            </div>

            {/* Error Message */}
            {errorMessage && (
              <div className="bg-red-50 border border-red-200 text-red-700 px-4 py-3 rounded">
                {errorMessage}
              </div>
            )}
          </form>
        </div>

        {/* Footer */}
        <div className="bg-gray-100 px-6 py-3 text-sm text-gray-600 border-t">
          <div className="flex justify-between">
            <span>CardDemo Administration System</span>
            <span>Press F3 to exit</span>
          </div>
        </div>
      </div>
    </div>
  );
}
